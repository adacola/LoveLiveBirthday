module Adacola.LoveLiveBirthday.Main

open System
open Argu
open Basis.Core
open MathNet.Numerics.Random

type CliArguments =
    | Trial of int
    | Uniform
    | Seed of int
    | Url of string
with
    interface IArgParserTemplate with
        member x.Usage: string = 
            match x with
            | Trial(_) -> "試行回数。省略時は100000"
            | Uniform -> "誕生日が選ばれる確率分布が一様分布とするモードで実行。省略時は出生数を元にした確率分布を使用"
            | Seed(_) -> "乱数のシード"
            | Url(_) -> "e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次一覧ページのURL"

let defaultTrial = 100000
let defaultUrl = "http://www.e-stat.go.jp/SG1/estat/GL08020102.do?_toGL08020102_&tclassID=000001041654&cycleCode=7&requestSender=estat"

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArguments>(errorHandler = ProcessExiter())
    let parseResult = parser.Parse args
    let trial = parseResult.GetResult(<@ Trial @>, defaultTrial)
    let isUniform = parseResult.Contains <@ Uniform @>
    let maybeSeed = parseResult.TryGetResult <@ Seed @>
    let yearContentUri = parseResult.GetResult(<@ Url @>, defaultUrl) |> Uri |> BirthCount.YearContentUri

    let random = maybeSeed |> Option.map Random.mersenneTwisterSeed |> Option.getOrElse Random.mersenneTwister
    let getBirthday lazyYearContent cache year =
        result {
            if isUniform then return Birthday.getRandomBirthday BirthCount.uniformBirthCount, cache else
            let! yearContent = lazyYearContent |> Lazy.value
            let! birthCount, cache = BirthCount.tryGetBirthCountOfAcademicYear yearContentUri yearContent cache year
            return Birthday.getRandomBirthday birthCount, cache
        }
    let getAllBirthCount lazyYearContent allYears =
        result {
            if isUniform then
                let birthCount = BirthCount.uniformBirthCount
                return Birthday.getRandomBirthday birthCount, Map.empty
            else
                let! yearContent = lazyYearContent |> Lazy.value
                let! allBirthCount, cache = BirthCount.tryGetAllBirthCounts yearContentUri yearContent allYears
                return Birthday.getRandomBirthday allBirthCount, cache
        }

    let func = 
        result {
            let allYears = [for i in 1996 .. 2015 -> i * 1<年>]
            let lazyYearContent = lazy(BirthCount.tryDownloadYearContent yearContentUri)
            let! getActorBirthday, cache = getAllBirthCount lazyYearContent allYears
            let! getμ's1stGradeBirthday, cache = getBirthday lazyYearContent cache LoveLive.μ's1stGradeYear
            let! getμ's2ndGradeBirthday, cache = getBirthday lazyYearContent cache LoveLive.μ's2ndGradeYear
            let! getμ's3rdGradeBirthday, cache = getBirthday lazyYearContent cache LoveLive.μ's3rdGradeYear
            let! getAqours1stGradeBirthday, cache = getBirthday lazyYearContent cache LoveLive.aqours1stGradeYear
            let! getAqours2ndGradeBirthday, cache = getBirthday lazyYearContent cache LoveLive.aqours2ndGradeYear
            let! getAqours3rdGradeBirthday, _ = getBirthday lazyYearContent cache LoveLive.aqours3rdGradeYear
            return fun random -> 
                let result, random =
                    LoveLive.getRandomBirthdays getActorBirthday getμ's1stGradeBirthday getμ's2ndGradeBirthday getμ's3rdGradeBirthday getActorBirthday getAqours1stGradeBirthday getAqours2ndGradeBirthday getAqours3rdGradeBirthday random
                result |> Birthday.getDuplicatedCount, random
        }
    match func with
    | Success f ->
        let result = (random, seq { 1 .. trial }) ||> Seq.mapFold (fun random _ -> f random) |> fst |> Seq.countBy id
        printfn "\n%d 回実行した結果:" trial
        result |> Seq.sortBy fst |> Seq.iter (fun (duplicatedCount, count) ->
            let ratio = float count / float trial * 100.
            printfn "かぶり%02d人 : %d回 (%.3f%%)" duplicatedCount count ratio)
        System.Console.ReadLine() |> ignore
        0
    | Failure e -> raise e
