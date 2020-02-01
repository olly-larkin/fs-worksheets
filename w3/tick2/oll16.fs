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
            | hd::tl when not <| List.contains hd ['.';'(';')';'[';']'] -> catchOther (acc @ [hd]) tl
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
            [Other (acc |> List.toArray |> String)] @ tokeniseRec tl
    Seq.toList str |> tokeniseRec

type AstT3 =
    | DotExp
    | RoundBraExp of AstT3
    | SqBraExp of AstT3
    | RoundSqBraExp of AstT3 * AstT3

let (|PTOKEN|_|) token lstRes =
    match lstRes with
    | Error _ -> lstRes
    | Ok (hd::tl) when snd hd = token -> Ok tl
    | Ok (hd::_) -> Error (fst hd, sprintf "Expected %A but got %A" token (snd hd))
    | _ -> Error (0, sprintf "Expected %A but no more tokens" token)
    |> Some

let rec (|PROUNDBRA|_|) lstRes =
    match lstRes with
    | Error _ -> None, lstRes
    | PTOKEN LRBrac (PEXP (expOpt, PTOKEN RRBrac lstRes')) ->
        match expOpt with
        | None -> None, lstRes'
        | Some exp -> Some exp, lstRes'
    | _ -> failwithf "Can't reach"
    |> Some

and (|PSQBRA|_|) lstRes =
    match lstRes with
    | Error _ -> None, lstRes
    | PTOKEN LSBrac (PEXP (expOpt, PTOKEN RSBrac lstRes')) ->
        match expOpt with
        | None -> None, lstRes'
        | Some exp -> Some exp, lstRes'
    | _ -> failwithf "Can't reach"
    |> Some

and (|PEXP|_|) lstRes =
    match lstRes with
    | Error _ -> None, lstRes
    | _ ->
        let dot = (|PTOKEN|_|) Dot lstRes |> Option.defaultWith (fun _ -> failwithf "Can't reach")
        let rndsqbra =
            match lstRes with
            | PROUNDBRA (Some exp1, PSQBRA (Some exp2, lstRes')) -> Some (RoundSqBraExp (exp1, exp2)), lstRes'
            | PROUNDBRA (_, PSQBRA (_, lstRes')) -> None, lstRes'
            | _ -> failwithf "Can't reach"
        let rndbra = (|PROUNDBRA|_|) lstRes |> Option.defaultWith (fun _ -> failwithf "Can't reach")
        let sqbra = (|PSQBRA|_|) lstRes |> Option.defaultWith (fun _ -> failwithf "Can't reach")
        match dot,rndsqbra,rndbra,sqbra with
        | (Ok lst), _, _, _ -> Some DotExp, Ok lst
        | _, (Some exp, Ok lst), _, _ -> Some exp, Ok lst
        | _, _, (Some exp, Ok lst), _ -> Some (RoundBraExp exp), Ok lst
        | _, _, _, (Some exp, Ok lst) -> Some (SqBraExp exp), Ok lst
        | _, _, (_, Error (i1, msg1)), (_, Error (i2, msg2)) ->
            if i1 >= i2
            then None, Error (i1, msg1)
            else None, Error (i2, msg2)
        | _ -> failwithf "Can't reach"
    |> Some

let parseT3 lst =
    match Ok (List.indexed lst) with
    | PEXP (Some exp, Ok []) -> Ok exp
    | PEXP (_, Ok ((i,_)::_)) -> Error (i, "Could not match all tokens")
    | PEXP (_, Error tup) -> Error tup
    | Ok ((_, hd)::_) -> Error (0, sprintf "Failed to match: Expected '.' or '(' or '[' but got %A" hd)
    | Ok [] -> Error (-1, "No tokens were given")
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
        ]
    runTests defaultConfig testLst |> ignore

[<EntryPoint>]
let main argv =
    runTokeniseTests
    runParserTests
    Console.ReadKey() |> ignore
    0 // return an integer exit code
