module Adacola.LoveLiveBirthday.Birthday

open System

/// 指定された誕生数の分布に基づいてランダムに誕生日を取得
let getRandomBirthday (BirthCount.BirthCount birthCounts) =
    let sumOfBirthCount = birthCounts |> Array.sum
    let integralBirthCounts =
        (-1, birthCounts) ||> Seq.scan (+) |> Seq.tail |> Seq.toArray
    let searchIndex n =
        match System.Array.BinarySearch(integralBirthCounts, n) with
        | i when i < 0 -> ~~~i
        | i -> i
    fun (random : Random) ->
        let result = random.Next(sumOfBirthCount) |> searchIndex
        result, random

/// 重複している要素の数を取得
let getDuplicatedCount xs = xs |> List.distinct |> List.length |> (-) xs.Length

/// 中の人の誕生日をランダムに取得
let generateActorRandomBirthdays getRandomBirthday (random : Random) memberCount =
    (random, [1 .. memberCount]) ||> List.mapFold (fun random _ -> getRandomBirthday random)
    
/// キャラの誕生日をランダムに取得
let generateCharactorRandomBirthdays getRandomBirthday (random : Random) memberCount birthdays =
    let rec loop results birthdays random =
        if memberCount <= List.length results then results, random else
        let result, random = getRandomBirthday random
        if Set.contains result birthdays then loop results birthdays random
        else loop (result::results) (Set.add result birthdays) random
    loop [] (set birthdays) random
