// Learn more about F# at http://fsharp.org

open System

let mode lst =
    let count =
        lst
        |> List.countBy id
        |> List.sortByDescending snd
    match count with
    | [] -> ([], 0)
    | (_, cnt)::_ ->
        let modes = List.takeWhile (fun (_, cnt') -> cnt' = cnt) count
        List.map fst modes , cnt

[<EntryPoint>]
let main argv =
    0 |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
