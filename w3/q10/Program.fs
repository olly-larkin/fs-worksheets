// Learn more about F# at http://fsharp.org

open System

let histogram (data: string) =
    data
    |> Seq.toList
    |> List.groupBy id
    |> Map.ofList
    |> Map.map (fun _ -> List.length) 

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
