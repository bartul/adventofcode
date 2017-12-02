#!/usr/local/bin/fsharpi
open System
open System.IO

let data = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")

let toNumberArray (str : string) = 
    str.Split([|'\t'; ' '|], StringSplitOptions.RemoveEmptyEntries)
let toIntArray = 
    Array.map int
let rowChecksum array =
    let max = Array.max array
    let min = Array.min array
    max - min

let result = Array.sumBy (toNumberArray >> toIntArray >> rowChecksum) data

printfn "Solution 1: %i" result

let rowChecksum2 array =
    let folder (candidate, a) item =
        let theOne = a |> Array.filter (fun x -> item % x = 0 && item <> x) |> Array.sum
        if (theOne = 0) then
            (candidate, a)
        else 
            (item / theOne, a)
    let (bingo, _) = Array.fold folder (0, array) array 
    bingo

let result2 = Array.sumBy (toNumberArray >> toIntArray >> rowChecksum2) data

printfn "Solution 2: %i" result2
   

// [|5; 9; 2; 8|] |> rowChecksum2
// [|9; 4; 7; 3|] |> rowChecksum2
// [|3; 8; 6; 5|] |> rowChecksum2

