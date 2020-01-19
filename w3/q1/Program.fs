// Learn more about F# at http://fsharp.org

open System

type Person = {
    Age: int;
    FirstName: string;
    FamilyName: string;
    Email: string
}

let newEmail em person = {person with Email = em}

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
