#!/usr/local/bin/fsharpi

open System.IO

let data =
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

type State = {
    TotalScore : int
    CurrentDepth : int
    InGarbage : bool
    IgnoreNext : bool
    GarbageCount : int
}
let folder state item = 
    match item, state.InGarbage, state.IgnoreNext with 
    | _, true, true -> { state with IgnoreNext = false }
    | '{', false, false -> { state with CurrentDepth = state.CurrentDepth + 1 }
    | '}', false, false -> { state with TotalScore = state.TotalScore + state.CurrentDepth;  CurrentDepth = state.CurrentDepth - 1 }
    | '<', false, false -> { state with InGarbage = true }
    | '>', true, false -> { state with InGarbage = false }
    | '!', true, false -> { state with IgnoreNext = true }
    | _, true, false -> { state with GarbageCount = state.GarbageCount + 1 }
    | _ -> state

let result = data |> Seq.fold folder { TotalScore = 0; CurrentDepth = 0; InGarbage = false; IgnoreNext = false; GarbageCount = 0 }

printfn "Solution 1: %i" result.TotalScore
printfn "Solution 2: %i" result.GarbageCount