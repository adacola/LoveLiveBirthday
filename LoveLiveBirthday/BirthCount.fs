﻿module Adacola.LoveLiveBirthday.BirthCount

open System
open System.Text
open System.Text.RegularExpressions
open System.Net
open Basis.Core

type YearContentUri = YearContentUri of Uri
type DataContentUri = DataContentUri of Uri
type BirthCountContentUri = BirthCountContentUri of Uri
type YearContent = YearContent of string
type DataContent = DataContent of string
type BirthCountContent = BirthCountContent of string
type BirthCount = BirthCount of int[]

let private tryDownload encoding toDestination (uri : Uri) =
    try
        printfn "%s をダウンロードします" uri.AbsoluteUri
        use client = new WebClient(Encoding = encoding)
        client.Headers.["User-Agent"] <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36"
        let content = client.DownloadString(uri)
        Threading.Thread.Sleep 2000
        content |> toDestination |> Success
    with e -> Failure e

let tryDownloadYearContent (YearContentUri uri) = tryDownload Encoding.UTF8 YearContent uri
let tryDownloadDataContent (DataContentUri uri) = tryDownload Encoding.UTF8 DataContent uri
let tryDownloadBirthCountCoutent (BirthCountContentUri uri) = tryDownload (Encoding.GetEncoding "Shift_JIS") BirthCountContent uri

let private failureParsing contentName = sprintf "%s のパーズに失敗しました" contentName |> exn |> Failure

/// e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次一覧ページから目的の年次のページのURIを取得
let tryParseYearContent (YearContentUri baseUri) (YearContent content) (year : int<年>) =
    match Regex.Match(content, sprintf """<a\s+href="(?<uri>[^"]+)"[^>]*>\s*%d年\s*</a>""" year, RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> DataContentUri |> Success
    | _ -> failureParsing baseUri.AbsoluteUri

/// e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次ページから出生年月日時のCSVのURIを取得
let tryParseDataContent (DataContentUri baseUri) (DataContent content) =
    match Regex.Match(content, """出生数[、，]出生年月日時・出[生産]の場所別.*?<a\s+href="(?<uri>[^"]+)"[^>]*>""", RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> BirthCountContentUri |> Success
    | _ -> failureParsing baseUri.AbsoluteUri

/// 出生年月日時のCSVから出生年月日ごとの誕生数の配列
let tryParseBirthCountContent (BirthCountContent content) =
    let rows = content.Split([|"\r\n"; "\n"; "\r"|], StringSplitOptions.None) |> Array.map (Str.splitBy ",")
    let tryGetDayBirthCount index =
        if Regex.IsMatch(rows.[index].[0], @"\A.+日\z") then
            rows.[index].[1] |> Int32.TryParse |> Option.ofTryByref
        else None
    let tryGetMonthBirthCounts monthIndex =
        Option.execute <| fun () ->
            monthIndex |> Seq.unfold (fun index ->
                tryGetDayBirthCount index |> Option.map (fun count -> count, index + 1))
            |> Seq.toArray
    let monthIndices = [|38; 71; 104; 137; 170; 203; 236; 269; 302; 335; 368; 401|]
    result {
        let! birthCountsTable =
            monthIndices |> Array.map tryGetMonthBirthCounts |> Array.validate
            |> Option.map Success |> Option.getOrElse (fun () -> failureParsing "出生年月日時のCSV")
        // うるう年対応。2/29の分は3/1に加算
        let result =
            if birthCountsTable.[1].Length = 29 then
                birthCountsTable |> Array.mapi (flip (fun xs -> function
                    | 1 -> xs.[.. 27]
                    | 2 -> Array.append [|xs.[0] + birthCountsTable.[1].[28]|] xs.[1 ..]
                    | _ -> xs))
            else birthCountsTable
        return result |> Array.concat |> BirthCount
    }

let tryGetBirthCountForcibly yearContentUri yearContent year =
    result {
        let! dataContentUri = tryParseYearContent yearContentUri yearContent year
        let! dataContent = tryDownloadDataContent dataContentUri
        let! birthCountContentUri = tryParseDataContent dataContentUri dataContent
        let! birthCountContent = tryDownloadBirthCountCoutent birthCountContentUri
        return! tryParseBirthCountContent birthCountContent
    }

let tryGetBirthCount yearContentUri yearContent cache year =
    result {
        match cache |> Map.tryFind year with
        | Some result -> return result, cache
        | None ->
            let! result = tryGetBirthCountForcibly yearContentUri yearContent year
            return result, cache |> Map.add year result
    }

let tryGetBirthCountOfAcademicYear yearContentUri yearContent cache (academicYear : int<年度>) =
    result {
        let firstYear = academicYear * 1<年/年度>
        let secondYear = firstYear + 1<年>
        let! (BirthCount firstBirthCount), cache = tryGetBirthCount yearContentUri yearContent cache firstYear
        let! (BirthCount secondBirthCount), cache = tryGetBirthCount yearContentUri yearContent cache secondYear
        // 4月2日～翌年の4月1日まで
        let result = Array.append secondBirthCount.[.. 90] firstBirthCount.[91 ..] |> BirthCount
        return result, cache
    }

let sumBirthCounts birthCounts =
    let add2BirthCounts (BirthCount birthCounts1) (BirthCount birthCounts2) =
        (birthCounts1, birthCounts2) ||> Array.map2 (+) |> BirthCount
    birthCounts |> Seq.reduce add2BirthCounts

let uniformBirthCount = Array.replicate 365 1 |> BirthCount

let tryGetAllBirthCounts yearContentUri yearContent years =
    let birthCountResults, cache =
        (Map.empty, years) ||> Seq.mapFold (fun cache year ->
            printfn "%d年の処理" year
            match tryGetBirthCount yearContentUri yearContent cache year with
            | Success(result, cache) -> Success result, cache
            | Failure e -> Failure e, cache)
    let cachedBirthCountResults = birthCountResults |> Seq.cache
    cachedBirthCountResults |> Seq.tryPick Result.toOptionFailure |> Option.map Failure |> Option.getOrElse (fun () ->
        let result = cachedBirthCountResults |> Seq.map Result.get |> sumBirthCounts
        Success(result, cache))
