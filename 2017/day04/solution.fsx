open System
open System.IO

let toStringArray (str : string) = 
    str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

let data = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.map toStringArray 

let hasNoDuplicates array =
    (array |> Array.distinct |> Array.length) = (array |> Array.length)

let result = data |> Array.filter hasNoDuplicates |> Array.length

printfn "Solution 1: %i" result

let orderByLetters (value : string) =  
    let a = value.ToCharArray() |> Array.sort
    String(a) |> string

let result2 = data |> Array.map (Array.map orderByLetters) |> Array.filter hasNoDuplicates |> Array.length

printfn "Solution 2: %i" result2

