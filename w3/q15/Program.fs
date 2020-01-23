// Learn more about F# at http://fsharp.org

open System

type TNoun = Noun of string
type TAdjective = Adjective of string
type TNounExp =  | AdjNoun of TAdjective * TNoun | JustNoun of TNoun

let (|PNOUN|_|) lst =
    match lst with
    | Ok (("dog" as w) :: lst')
    | Ok (("cat" as w) :: lst') -> Some (Noun w), Ok lst'
    | _ -> None , Error <| sprintf "%A found when Noun was expected" lst
    |> Some

let (|PADJECTIVE|_|) lst =
    match lst with
    | Ok (("red" as a) :: lst')
    | Ok (("green" as a) :: lst')
    | Ok (("blue" as a) :: lst') -> Some (Adjective a) , Ok lst'
    | _ -> None , Error <| sprintf "%A found when adjective was expected" lst
    |> Some

let (|PNOUNEXP|_|) lst =
    match lst with
    | Error _ -> None, lst
    | PNOUN (Some n, lst') -> Some (JustNoun n), lst'
    | PADJECTIVE (Some a, PNOUN (Some n, lst')) -> Some (AdjNoun (a, n)), lst'
    | _ -> None, Error <| sprintf "%A can't be parsed as a Noun Expression" lst
    |> Some

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
