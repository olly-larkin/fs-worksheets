// Learn more about F# at http://fsharp.org

open System

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

let (|PTOKEN|_|) token =
    function
    | hd::tl when hd = token -> Some tl
    | _ -> None

let rec (|PROUNDBRA|_|) =
    function
    | PTOKEN LRBrac (PEXP (exp, PTOKEN RRBrac tl)) -> Some (exp, tl)
    | _ -> None 

and (|PSQBRA|_|) =
    function
    | PTOKEN LSBrac (PEXP (exp, PTOKEN RSBrac tl)) -> Some (exp, tl)
    | _ -> None 

and (|PEXP|_|) =
    function
    | Dot::tl -> Some (DotExp, tl)
    | PROUNDBRA (exp1, PSQBRA (exp2, tl)) -> Some (RoundSqBraExp (exp1,exp2), tl)
    | PROUNDBRA (exp, tl) -> Some (RoundBraExp exp, tl)
    | PSQBRA (exp, tl) -> Some (SqBraExp exp, tl)
    | _ -> None

[<EntryPoint>]
let main argv =
    ".abc d)" |> tokeniseT3 |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
