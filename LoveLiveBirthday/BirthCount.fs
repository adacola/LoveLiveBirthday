module Adacola.LoveLiveBirthday.BirthCount

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
type BirthCounts = BirthCounts of int[]

let private tryDownload encoding toDestination (uri : Uri) =
    async {
        try
            use client = new WebClient(Encoding = encoding)
            let! content = client.AsyncDownloadString(uri)
            return content |> toDestination |> Success
        with e -> return Failure e
    }

let tryDownloadYearContent (YearContentUri uri) = tryDownload Encoding.UTF8 YearContent uri
let tryDownloadDataContent (DataContentUri uri) = tryDownload Encoding.UTF8 DataContent uri
let tryDownloadBirthCountCoutent (BirthCountContentUri uri) = tryDownload (Encoding.GetEncoding "Shift_JIS") BirthCountContent uri

/// e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次一覧ページから目的の年次のページのURIを取得
let tryGetYearUri (YearContentUri baseUri) (YearContent content) (year : int<年>) =
    match Regex.Match(content, sprintf """<a\s+href="(?<uri>[^"]+)"[^>]*>\s*%d年\s*</a>""" year, RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> DataContentUri |> Some
    | _ -> None

/// e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次ページから出生年月日時のCSVのURIを取得
let tryGetDataUri (DataContentUri baseUri) (DataContent content) =
    match Regex.Match(content, """出生数，出生年月日時・出生の場所別.*?<a\s+href="(?<uri>[^"]+)"[^>]*>""", RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> BirthCountContentUri |> Some
    | _ -> None

/// 出生年月日時のCSVから出生年月日ごとの誕生数の配列
let tryGetBirthCounts (BirthCountContent content) =
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
    option {
        let! birthCountsTable = monthIndices |> Array.map tryGetMonthBirthCounts |> Array.validate
        // うるう年対応。2/29の分は3/1に加算
        let result =
            if birthCountsTable.[1].Length = 29 then
                birthCountsTable |> Array.mapi (flip (fun xs -> function
                    | 1 -> xs.[.. 27]
                    | 2 -> Array.append [|xs.[0] + birthCountsTable.[1].[28]|] xs.[1 ..]
                    | _ -> xs))
            else birthCountsTable
        return result |> Array.concat |> BirthCounts
    }
