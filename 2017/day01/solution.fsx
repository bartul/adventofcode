open System
open System.IO

let toCharArray (str : string) = 
    str.Trim().ToCharArray()
let toIntArray = 
    Array.map (fun c -> Int32.Parse(c.ToString()))

let data = 
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")
    |> toCharArray 
    |> toIntArray

type Result = { 
    PreviousValue : int
    Sum : int
}

let startingResult = { PreviousValue = data |> Array.rev |> Array.head; Sum = 0 }
let folder state item =
    if item = state.PreviousValue then 
        { state with Sum = state.Sum + item; PreviousValue = item }
    else 
        { state with PreviousValue = item }

let result = data |> Array.fold folder startingResult 

printfn "Solution 1: %i" result.Sum

type Result2 = { 
    Position : int
    Sum : int
}

let halfWayPosition length position =
    let halfLength = length / 2
    if position > halfLength then   
        position - halfLength
    else
        position + halfLength

let halfWayValueGetter collection position =
    let halfWayPosition = halfWayPosition (Array.length collection) position
    collection.[halfWayPosition - 1]

let startingResult2 = { Position = 1 ; Sum = 0; }
let folder2 data state item =
    let nextValue = halfWayValueGetter data state.Position
    if item = nextValue then 
        { state with Sum = state.Sum + item; Position = state.Position + 1; }
    else 
        { state with Position = state.Position + 1; }

let result2 = data |> Array.fold (folder2 data) startingResult2 

printfn "Solution 2: %i" result2.Sum
