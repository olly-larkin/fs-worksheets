// Learn more about F# at http://fsharp.org

open System

let gap =
    List.sort
    >> List.pairwise
    >> List.map (fun (a,b) -> b - a)
    >> List.max

[<EntryPoint>]
let main argv =
    gap [1;3;6;3;2] |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
