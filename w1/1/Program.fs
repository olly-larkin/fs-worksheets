open System

let print x =
    printfn "%A" x

let double = ((*) 2)
let doubleList = List.map double

[<EntryPoint>]
let main argv =
    print "Hello, world!"
    print (doubleList [1;2;5])
    Console.ReadKey() |> ignore
    0