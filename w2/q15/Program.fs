// Learn more about F# at http://fsharp.org

open System

let rec subsets lst =
    match lst with
    | hd::tl ->
        List.allPairs [[hd];[]] (subsets tl)
        |> List.map (fun (x,y) -> List.append x y) 
    | [] -> [[]]

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
