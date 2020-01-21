// Learn more about F# at http://fsharp.org

open System

let primeFactors n =
    let rec primeRecurse c n = 
        if c * c > n
        then [n]
        else
            if n % c = 0
            then c :: (primeRecurse c (n/c))
            else primeRecurse (c+1) n
    n |> primeRecurse 2

[<EntryPoint>]
let main argv =
    primeFactors 12 |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
