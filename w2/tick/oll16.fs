// Learn more about F# at http://fsharp.org

open System

let lexNGram (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option=
    let takeIfInChars chars (acc,lst) : (char list * char list) option =
        match lst with
        | hd::tl when List.contains hd chars -> Some (acc @ [hd], tl)
        | _ -> None

    let rec takeWhileInChars firstVisit chars (acc,lst)  =
        match lst with
        | hd::tl when List.contains hd chars -> (acc @ [hd], tl) |> takeWhileInChars false chars
        | _ when firstVisit -> None
        | _ -> Some (acc, lst)

    let tryMatch state (charsLst,canRepeat) =
        if canRepeat
        then Option.defaultValue ([],[]) state |> takeWhileInChars true charsLst
        else Option.defaultValue ([],[]) state |> takeIfInChars charsLst 

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
            Seq.toList "\"rabbits 15.7"
            Seq.toList "11"
            Seq.toList "15. 7"
            Seq.toList "15.7"
        ]
    ((), testBatch) ||> List.fold (fun _ lst -> lexNGram stringLit lst |> printfn "%A") |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
