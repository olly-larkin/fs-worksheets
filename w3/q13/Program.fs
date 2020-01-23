// Learn more about F# at http://fsharp.org

open System

let memoise fn =
    let mutable cache = Map []
    fun x ->
        match Map.tryFind x cache with
        | Some y -> y
        | None ->
            let res = fn x
            cache <- Map.add x res cache
            res

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
