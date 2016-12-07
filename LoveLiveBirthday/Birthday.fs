module Adacola.LoveLiveBirthday.Birthday

open System

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

let getDuplicatedCount xs = xs |> List.distinct |> List.length |> (-) xs.Length

let generateActorRandomBirthdays getRandomBirthday (random : Random) memberCount =
    (random, [1 .. memberCount]) ||> List.mapFold (fun random _ -> getRandomBirthday random)
    
let generateCharactorRandomBirthdays getRandomBirthday (random : Random) memberCount birthdays =
    let rec loop results birthdays random =
        if memberCount <= List.length results then results, random else
        let result, random = getRandomBirthday random
        if Set.contains result birthdays then loop results birthdays random
        else loop (result::results) (Set.add result birthdays) random
    loop [] (set birthdays) random
