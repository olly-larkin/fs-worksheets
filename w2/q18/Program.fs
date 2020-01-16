// Learn more about F# at http://fsharp.org

open System

let subsets lst =
    let rec subsets' l subs =
        match l with
        | hd::tl ->
            List.allPairs subs [[hd];[]]
            |> List.map ((<||) List.append)
            |> subsets' tl
        | [] -> [[]]
    subsets' lst [[]]

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
