// Learn more about F# at http://fsharp.org

open System

let rec merge lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | hd::tl1 ->
        let pos, tl2 = List.partition (fun x -> x < hd) lst2
        pos @ [hd] @ (merge tl1 tl2)
    
let rec sort lst =
    match List.length lst with
    | 0 | 1 -> lst
    | _ ->
        let left, right = List.splitAt (List.length lst / 2) lst
        (sort left, sort right) ||> merge

[<EntryPoint>]
let main argv =
    sort [5;2;6;8;5;3;2;5;7;8] |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
