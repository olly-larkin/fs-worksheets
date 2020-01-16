// Learn more about F# at http://fsharp.org

open System

let reverseList list =
    List.fold (fun lst elem -> elem::lst) [] list  
 
[<EntryPoint>]
let main argv =
    reverseList [1;2;3;4] |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
