// Learn more about F# at http://fsharp.org

open System

let negPosOr0 list =
    List.tryFind ((>) 0) list
    |> Option.orElseWith (fun () -> List.tryFind ((<) 0) list)
    |> Option.defaultValue 0

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
