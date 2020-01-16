// Learn more about F# at http://fsharp.org

open System

let sequences a b =
    List.allPairs [a..b] [a..b]
    |> List.collect (fun (e1, e2) -> if (e1 < e2) then [[e1..e2]] else [])

[<EntryPoint>]
let main argv =
    sequences 2 6 |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
