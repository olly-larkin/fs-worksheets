// Learn more about F# at http://fsharp.org

open System

let insertElement index elem lst =
    let first, second = List.splitAt index lst
    first @ [elem] @ second

let insertList index newLst lst =
    let first, second = List.splitAt index lst
    first @ newLst @ second

[<EntryPoint>]
let main argv =
    insertElement 3 5 [0;1;2;3;4;5;6;7;8] |> printfn "%A"
    insertList 3 [10;11] [0;1;2;3;4;5;6;7;8] |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
