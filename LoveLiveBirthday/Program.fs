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
            | Trial(_) -> "試行回数。省略時は1000"
            | Uniform -> "誕生日が選ばれる確率分布が一様分布とするモードで実行。省略時は出生数を元にした確率分布を使用"
            | Seed(_) -> "乱数のシード"
            | Url(_) -> "e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次一覧ページのURL"

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArguments>()
    let parseResult = parser.Parse args
    let trial = parseResult.GetResult(<@ Trial @>, 1000)
    let isUniform = parseResult.Contains <@ Uniform @>
    let maybeSeed = parseResult.TryGetResult <@ Seed @>
    let yearContentUri =
        parseResult.TryGetResult <@ Url @>
        |> Option.getOrElse (fun () -> "http://www.e-stat.go.jp/SG1/estat/GL08020102.do?_toGL08020102_&tclassID=000001041654&cycleCode=7&requestSender=estat")
        |> Uri |> BirthCount.YearContentUri

    let random = maybeSeed |> Option.map Random.mersenneTwisterSeed |> Option.getOrElse Random.mersenneTwister
    let getBirthday cache year =
        result {
            let! birthCount, cache =
                if isUniform then Success(BirthCount.uniformBirthCount, cache)
                else BirthCount.tryGetBirthCountOfAcademicYear yearContentUri cache year
            return Birthday.getRandomBirthday birthCount, cache
        }

    let func = 
        result {
            let allYears = [for i in 1995 .. 2015 -> i * 1<年>]
            let! allBirthCount, cache = BirthCount.tryGetAllBirthCounts yearContentUri allYears
            let getActorBirthday = Birthday.getRandomBirthday (if isUniform then BirthCount.uniformBirthCount else allBirthCount)
            let! getμ's1stGradeBirthday, cache = getBirthday cache LoveLive.μ's1stGradeYear
            let! getμ's2ndGradeBirthday, cache = getBirthday cache LoveLive.μ's2ndGradeYear
            let! getμ's3rdGradeBirthday, cache = getBirthday cache LoveLive.μ's3rdGradeYear
            let! getAqours1stGradeBirthday, cache = getBirthday cache LoveLive.aqours1stGradeYear
            let! getAqours2ndGradeBirthday, cache = getBirthday cache LoveLive.aqours2ndGradeYear
            let! getAqours3rdGradeBirthday, _ = getBirthday cache LoveLive.aqours3rdGradeYear
            return fun random -> 
                let result, random =
                    LoveLive.getRandomBirthdays getActorBirthday getμ's1stGradeBirthday getμ's2ndGradeBirthday getμ's3rdGradeBirthday getActorBirthday getAqours1stGradeBirthday getAqours2ndGradeBirthday getAqours3rdGradeBirthday random
                result |> Birthday.getDuplicatedCount, random
        }
    match func with
    | Success f ->
        let result = (random, seq { 1 .. trial }) ||> Seq.mapFold (fun random _ -> f random) |> fst |> Seq.countBy id
        printfn "%d 回実行した結果:" trial
        result |> Seq.sortBy fst |> Seq.iter ((<||) (printfn "かぶり%02d人 : %d回"))
        System.Console.ReadLine() |> ignore
        0
    | Failure e -> raise e
