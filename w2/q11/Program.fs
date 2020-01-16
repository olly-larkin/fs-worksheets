// Learn more about F# at http://fsharp.org

open System

let fact n = List.fold (*) 1 [2 .. n]

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
