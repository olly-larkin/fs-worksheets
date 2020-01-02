open System

let div (x: float) y = x / y
let recip = div 1.0

[<EntryPoint>]
let main argv =
    printfn "%A" (recip 10.0)
    Console.ReadKey() |> ignore
    0