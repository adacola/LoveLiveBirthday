module Adacola.LoveLiveBirthday.Birthday

open MathNet.Numerics.Random

let private random = Random.mersenneTwister()

let getRandomBirthday (BirthCount.BirthCount birthCounts) =
    let sumOfBirthCount = birthCounts |> Array.sum
    let integralBirthCounts =
        (-1, birthCounts) ||> Seq.scan (+) |> Seq.tail |> Seq.toArray
    let searchIndex n =
        match System.Array.BinarySearch(integralBirthCounts, n) with
        | i when i < 0 -> ~~~i
        | i -> i
    fun () -> random.Next(sumOfBirthCount) |> searchIndex

let getDuplicatedCount xs = xs |> List.distinct |> List.length |> (-) xs.Length

let generateActorRandomBirthdays getRandomBirthday memberCount =
    [for _ in 1 .. memberCount -> getRandomBirthday()]
    
let generateCharactorRandomBirthdays getRandomBirthday memberCount birthdays =
    set birthdays |> Seq.unfold (fun birthdays ->
        let birthday = getRandomBirthday()
        if Set.contains birthday birthdays then Some(None, birthdays) else Some(Some birthday, Set.add birthday birthdays))
    |> Seq.choose id |> Seq.take memberCount |> Seq.toList
