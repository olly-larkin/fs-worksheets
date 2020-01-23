// Learn more about F# at http://fsharp.org

open System

type Repeat = RuleOneMany | RuleOnce | RuleZeroMany

let lexNGram (ngram: (char list * Repeat) list) (cLst: char list) : (char list * char list) option=
    let takeOnce chars (acc, lst) : (char list * char list) option =
        match lst with
        | hd::tl when List.contains hd chars -> Some (acc @ [hd], tl)
        | _ -> None

    let rec takeOneMany firstVisit chars (acc, lst)  =
        match lst with
        | hd::tl when List.contains hd chars -> (acc @ [hd], tl) |> takeOneMany false chars
        | _ when firstVisit -> None
        | _ -> Some (acc, lst)

    let rec takeZeroMany chars (acc, lst) =
        match lst with
        | hd::tl when List.contains hd chars -> (acc @ [hd], tl) |> takeZeroMany chars
        | _ -> Some (acc, lst)

    let tryMatch state (charsLst,repeat) = 
        match repeat with
        | RuleOneMany -> Option.defaultValue ([],[]) state |> takeOneMany false charsLst
        | RuleOnce -> Option.defaultValue ([],[]) state |> takeOnce charsLst 
        | RuleZeroMany -> Option.defaultValue ([],[]) state |> takeZeroMany charsLst

    (Some ([], cLst), ngram) ||> List.fold tryMatch

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
    [['0'..'9'],RuleZeroMany
     ['.';','],RuleOnce
     ['0'..'9'],RuleOneMany]

let integerLit =
    [['0'..'9'],RuleOneMany]

let stringLit =
    [['\"'],RuleOnce
     ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],RuleZeroMany
     ['\"'],RuleOnce]

let combinedLexers =
    [decimalLit;integerLit;stringLit]
    |> List.map lexNGram
    |> List.reduce (<|>)

[<EntryPoint>]
let main argv =
    let testBatch =
        [
            Seq.toList "15.7 rabbits"
            Seq.toList "\"rabbits\" 15.7"
            Seq.toList "\"rabbits 15.7"
            Seq.toList "11"
            Seq.toList "15. 7"
            Seq.toList "15.7"
            Seq.toList "\"\""
        ]
    ((), testBatch) ||> List.fold (fun _ lst -> lexNGram stringLit lst |> printfn "%A") |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
