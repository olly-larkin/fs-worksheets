open System
open FsCheck
open Expecto
    
let revOfRevIsOrig (x: int list) = List.rev (List.rev x) = x
    
let listsAreSmall (x:int list) = List.length x < 10 // this test should fail
    
// Tests are written as individual Expecto Test values, each tagged with [<Tests>]
// The code demonstrates different ways to run Expecto and FsCheck tests
    
[<Tests>]
let simpleExpectoTest =
    testCase "A simple test" <| fun () ->
        let expected = 4
        Expect.equal (2+2) expected "2+2 = 4"
    
[<Tests>]
let expectoFsCheckTest1 = 
    testProperty "Reverse of reverse of a list is the original list" <|
        fun (xs:list<int>) -> List.rev (List.rev xs) = xs
        // NB this is revOfRevIsOrig written out as anonymous function
    
[<Tests>]
let expectoFsCheckTest2 = 
    testProperty "Lists are small" <|
        listsAreSmall
    
let fsCheckConfig = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    
[<Tests>]
let expectoCheckTestFsCheckWithConfig1 =
    // you can also override the FsCheck config
    testPropertyWithConfig fsCheckConfig "Product is distributive over addition" <|
        fun a b c ->
            a * (b + c) = a * b + a * c
    
let testsWithoutExpecto() =
    Check.Quick revOfRevIsOrig
    Check.Quick listsAreSmall
    Check.Quick (4 = 2+2)
    
let testListWithExpecto =
    testList "A test group" [
        simpleExpectoTest
        expectoFsCheckTest1
        expectoFsCheckTest2
        expectoCheckTestFsCheckWithConfig1
    ]
    
let testsWithExpecto() =
    runTests defaultConfig testListWithExpecto |> ignore
    
let allTestsWithExpecto() =
    runTestsInAssembly defaultConfig [||]
    
[<EntryPoint>]
let main argv =
    printfn "Testing with FSCheck and Expecto!"
    // testsWithoutExpecto() |> ignore
    // testsWithExpecto() |> ignore
    // allTestsWithExpecto() |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code