// Learn more about F# at http://fsharp.org

open System

let rec filter f lst =
    match lst with
    | hd::tl when f hd -> hd::(filter f tl)
    | _::tl -> filter f tl
    | [] -> []

[<EntryPoint>]
let main argv =
    filter ((<) 0) [1;-1;10;5;-5] |> printfn "%A" 
    Console.ReadKey() |> ignore
    0 // return an integer exit code
