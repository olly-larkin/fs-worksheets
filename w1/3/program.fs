open System

let print x =
    printfn "%A" x

let divBy2 = (/) 2.0

[<EntryPoint>]
let main argv =
    print (divBy2 5.0)
    Console.ReadKey() |> ignore
    0