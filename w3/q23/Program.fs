// Learn more about F# at http://fsharp.org

open System

type Player = PlayerOne | PlayerTwo

/// The point score in for a player in a game
type PlayerPoints = Love | Fifteen | Thirty | Forty 

/// The score of a game
type TennisGameScore = 
    | Points of PlayerPoints * PlayerPoints 
    | Advantage of Player 
    | Deuce 

type MaybeWonTennisGameScore = Win of Player | Game of TennisGameScore

let scorePoint player gameScore =
    match gameScore with
    | Advantage p when p = player -> Win player
    | Advantage _ -> Game Deuce
    | Deuce -> Game <| Advantage player
    | Points (p1, p2) -> failwithf "Implement"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
