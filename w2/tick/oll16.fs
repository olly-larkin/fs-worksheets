// Learn more about F# at http://fsharp.org

open System

type State<'a> =
    | Running of ('a list * 'a list)
    | Finished of ('a list * 'a list)

module State =
    let toOption x =
        match x with
        | Finished ([], _) -> None
        | Finished arrs -> Some arrs
        | Running ([], _) -> None
        | Running arrs -> Some arrs

let lexNGram (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option=
    let takeIfInChars chars (acc,lst) : (char list * char list) option =
        match lst with
        | [] -> None
        | hd::tl ->
            match List.tryFind ((=) hd) chars with
            | None -> None
            | _ -> Some (acc @ [hd], tl)
    let rec takeWhileInChars chars (acc,lst) =
        match lst with
        | [] -> (acc, lst)
        | hd::tl ->
            match List.tryFind ((=) hd) chars with
            | None -> (acc, lst)
            | _ -> (acc @ [hd], tl) |> takeWhileInChars chars
    let tryMatch state (charsLst,canRepeat) =
        match state with
        | Finished _ -> state
        | Running lsts ->
            match takeIfInChars charsLst lsts with
            | None -> Finished lsts
            | Some lsts' ->
                if canRepeat
                then Running (takeWhileInChars charsLst lsts')
                else Running lsts' 
    (Running ([], cLst), ngram) ||> List.fold tryMatch |> State.toOption

type Lexer = char list -> (char list * char list) option

let (<|>) (lex1: Lexer) (lex2: Lexer): Lexer =
    fun cLst ->
        (lex1 cLst, lex2 cLst) ||> Option.orElse

let (>=>) (lex1: Lexer) (lex2: Lexer): Lexer =
    fun cLst ->
        match lex1 cLst with
        | None -> None
        | Some (tokL, matchL) ->
            match lex2 matchL with
            | None -> Some (tokL, matchL)
            | Some (tokL', matchL') -> Some (tokL @ tokL', matchL')

let decimalLit =
    [['0'..'9'],true
     ['.';','],false
     ['0'..'9'],true]

let integerLit =
    [['0'..'9'],true]

let stringLit =
    [['\"'],false
     ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],true
     ['\"'],false]

let emptyStringLit =
    [['\"'],false
     ['\"'],false]

let combinedLexers =
    [decimalLit;integerLit;emptyStringLit;stringLit]
    |> List.map lexNGram
    |> List.reduce (<|>)

[<EntryPoint>]
let main argv =
    let testBatch =
        [
            Seq.toList "15.7 rabbits"
            Seq.toList "\"rabbits\" 15.7"
            Seq.toList "11"
            Seq.toList "15. 7"
        ]
    ((), testBatch) ||> List.fold (fun _ lst -> (lexNGram integerLit <|> lexNGram stringLit) lst |> printfn "%A") |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
