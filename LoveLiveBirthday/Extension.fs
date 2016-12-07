namespace Adacola.LoveLiveBirthday

open Basis.Core

[<AutoOpen>]
module Util =
    let flip f x y = f y x

module Option =
    let execute f = try Some(f()) with e -> None
    let ofTryByref = function true, r -> Some r | false, _ -> None

module Array =
    let validate xs =
        let acc = ResizeArray(Array.length xs)
        (Some(acc), xs) ||> Array.fold (fun acc x ->
            option {
                let! results = acc
                let! result = x
                do results.Add result
                return results
            })
        |> Option.map Seq.toArray
