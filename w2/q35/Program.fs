﻿// Learn more about F# at http://fsharp.org

open System

let printTable tab =
    tab |> List.map (fun x -> printf "%A\n" x; x)

let tabulate tab =
    let newList n v = [1 .. n] |> List.map (fun _ -> v)
    let colWidth =
        let colWidthFold acc arr = 
            let newAcc = List.append acc <| newList (List.length arr - List.length acc) 0
            let newArr = List.append arr <| newList (List.length acc - List.length arr) ""
            ([], List.zip newArr newAcc) ||> List.fold (fun acc (str, size) -> acc @ [List.max [size; String.length str]])
        ([], tab) ||> List.fold colWidthFold
    let tabulateFold acc arr =
        let extendStringsFold acc (width, str) =
            acc @ [String.concat "" [str; String.replicate (width - String.length str) " "]]
        acc @ [([], arr @ List.replicate (List.length colWidth - List.length arr) "" |> List.zip colWidth) ||> List.fold extendStringsFold]
    ([], tab) ||> List.fold tabulateFold

[<EntryPoint>]
let main argv =
    [
        ["aa" ; "abc"]
        ["abcde"]
        ["abc"; "a"; "axxx"]
    ] 
    |> tabulate 
    |> printTable
    |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
