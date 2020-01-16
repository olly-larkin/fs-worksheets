// Learn more about F# at http://fsharp.org

open System

let length lst =
    (0, lst) ||> List.fold (fun st _ -> st + 1)

[<EntryPoint>]
let main argv =
    length [1;2;3;4] |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
