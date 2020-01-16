// Learn more about F# at http://fsharp.org

open System

let insertElement i x (lst: 'a list) =
    lst.[..i-1] @ [x] @ lst.[i..]

[<EntryPoint>]
let main argv =
    insertElement 3 10 [0;1;2;3;4;5;6;7] |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
