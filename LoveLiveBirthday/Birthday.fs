module Adacola.LoveLiveBirthday.Birthday

open MathNet.Numerics.Random

let private random = Random.mersenneTwister()

/// 2014年の日別の出生数に基いて誕生日のインデックスをランダムに出力する。
/// 1月1日を0、1月2日を1、……、12月31日を364としたインデックスを返す。
let getRandomBirthday (BirthCount.BirthCounts birthCounts) =
    let sumOfBirthCount = birthCounts |> Array.sum
    let integralBirthCounts =
        (-1, birthCounts) ||> Seq.scan (fun sum birthCount -> sum + birthCount) |> Seq.tail |> Seq.toArray
    let searchIndex n =
        match System.Array.BinarySearch(integralBirthCounts, n) with
        | i when i < 0 -> ~~~i
        | i -> i
    fun () -> random.Next(sumOfBirthCount) |> searchIndex

let isUnique xs = xs |> List.distinct |> (=) xs

let generateActorRandomBirthdays getRandomBirthday memberCount =
    [for _ in 1 .. memberCount -> getRandomBirthday()]
    
let generateCharactorRandomBirthdays getRandomBirthday memberCount birthdays =
    set birthdays |> Seq.unfold (fun birthdays ->
        let birthday = getRandomBirthday()
        if Set.contains birthday birthdays then Some(None, birthdays) else Some(Some birthday, Set.add birthday birthdays))
    |> Seq.choose id |> Seq.take memberCount |> Seq.toList
