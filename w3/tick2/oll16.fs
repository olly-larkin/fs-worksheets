// Learn more about F# at http://fsharp.org

open System
open Expecto

type Token = 
    | Other of string
    | Dot
    | LRBrac
    | RRBrac
    | LSBrac
    | RSBrac

let tokeniseT3 (str: string) = 
    let rec tokeniseRec lst =
        let rec catchOther acc lst =
            match lst with
            | hd::tl when not <| List.contains hd ['.';'(';')';'[';']'] -> catchOther (acc @ [string hd]) tl
            | _ -> acc, lst
        match lst with
        | [] -> []
        | '.'::tl -> [Dot] @ tokeniseRec tl
        | '('::tl -> [LRBrac] @ tokeniseRec tl
        | ')'::tl -> [RRBrac] @ tokeniseRec tl
        | '['::tl -> [LSBrac] @ tokeniseRec tl
        | ']'::tl -> [RSBrac] @ tokeniseRec tl
        | _ -> 
            let acc, tl = catchOther [] lst
            Other (List.reduce (+) acc) :: tokeniseRec tl
    Seq.toList str |> tokeniseRec

type AstT3 =
    | DotExp
    | RoundBraExp of AstT3
    | SqBraExp of AstT3
    | RoundSqBraExp of AstT3 * AstT3

let (|PTOKEN|_|) token inp =
    match inp with
    | Error _ -> inp
    | Ok [] -> Error (0, sprintf "Expected '%A' but no more tokens" token)
    | Ok (hd::tl) when hd = token -> Ok tl
    | Ok inp' -> Error (List.length inp' , sprintf "'%A' expected but got '%A'" token (List.head inp'))
    |> Some

let rec (|PROUNDBRA|_|) inp =
    match inp with
    | Error _ -> None, inp
    | PTOKEN LRBrac (PEXP (astOp, PTOKEN RRBrac inp')) -> astOp, inp'
    | _ -> failwithf "Can't reach"
    |> Some

and (|PSQBRA|_|) inp =
    match inp with
    | Error _ -> None, inp
    | PTOKEN LSBrac (PEXP (astOp, PTOKEN RSBrac inp')) -> astOp, inp'
    | _ -> failwithf "Can't reach"
    |> Some

and (|PROUNDSQBRA|_|) inp =
    match inp with
    | Error _ -> (None, None), inp
    | PROUNDBRA (astOp1, PSQBRA (astOp2, tl)) -> (astOp1, astOp2), tl
    | _ -> failwithf "Can't reach"
    |> Some

and (|PEXP|_|) inp =
    match inp with
    | Error _ -> None, inp
    | _ ->
        let unwrapOp op = Option.defaultWith (fun _ -> failwithf "Should always be Some") op
        let dot = (|PTOKEN|_|) Dot inp |> unwrapOp
        let rndsqbra = (|PROUNDSQBRA|_|) inp |> unwrapOp
        let rndbra = (|PROUNDBRA|_|) inp |> unwrapOp
        let sqbra = (|PSQBRA|_|) inp |> unwrapOp
        match dot,rndsqbra,rndbra,sqbra with
        | Error _, (_, Error _), (_, (Error (i1, msg1) as e1)), (_, (Error (i2, msg2) as e2)) ->
            if i1 <= i2
            then None, e1
            else None, e2
        | _, (_, (Error (i, msg) as e)), (Some ast, Ok tl), _ when i < List.length tl -> None, e
        | Ok tl as rest, _, _, _ -> Some DotExp, rest
        | _, ((Some ast1, Some ast2), rest), _, _ -> Some (RoundSqBraExp (ast1, ast2)), rest
        | _, _, (Some ast, rest), _ -> Some (RoundBraExp ast), rest
        | _, _, _, (Some ast, rest) -> Some (SqBraExp ast), rest
        | _ -> failwithf "Can't reach" 
    |> Some

let parseT3 inp =
    match Ok inp with 
    | PEXP (Some ast, Ok []) -> Ok ast
    | PEXP (Some _, Ok inp') -> Error (List.length inp - List.length inp', "Could not parse all input tokens")
    | PEXP (_, Error (i, msg)) -> Error (List.length inp - i, msg)
    | _ -> failwithf "Can't reach"

[<Tests>]
let tokenise1 =
    testCase "tokenise test 1" <| fun () ->
        Expect.equal (tokeniseT3 "abc..") [Other "abc"; Dot; Dot] "abc..  -->  [Other \"abc\", Dot, Dot]"

[<Tests>]
let tokenise2 =
    testCase "tokenise test 2" <| fun () ->
        Expect.equal (tokeniseT3 "((.))") [LRBrac; LRBrac; Dot; RRBrac; RRBrac] "((.))  -->  [LRBrac; LRBrac; Dot; RRBrac; RRBrac]"

[<Tests>]
let parse1 =
    testCase "parser test 1" <| fun () ->
        Expect.equal (parseT3 [LRBrac; LRBrac; Dot; RRBrac; RRBrac]) (Ok (RoundBraExp (RoundBraExp (DotExp)))) "[LRBrac; LRBrac; Dot; RRBrac; RRBrac]  -->  Ok (RoundBraExp (RoundBraExp (DotExp)))"

[<Tests>]
let parse2 =
    testCase "parser test 2" <| fun () ->
        Expect.equal (parseT3 [LRBrac; LRBrac; Dot; RRBrac; RRBrac; LSBrac; LRBrac; Dot; RSBrac]) (Error (8, "'RRBrac' expected but got 'RSBrac'")) "[LRBrac; LRBrac; Dot; RRBrac; RRBrac; LSBrac; LRBrac; Dot; RSBrac]  -->  Error (9, \"'RRBrac' expected but got 'RSBrac'\")"

let runTokeniseTests =
    let testLst =
        testList "Tokenise Test Group" [
            tokenise1
            tokenise2
        ]
    runTests defaultConfig testLst |> ignore

let runParserTests =
    let testLst =
        testList "Parser Test Group" [
            parse1
            parse2
        ]
    runTests defaultConfig testLst |> ignore

[<EntryPoint>]
let main argv =
    runTokeniseTests
    runParserTests
    Console.ReadKey() |> ignore
    0 // return an integer exit code
