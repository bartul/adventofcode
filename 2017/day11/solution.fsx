#!/usr/local/bin/fsharpi

open System
open System.IO

let parseMovement = function
    | "n" -> fun (x, y) -> x, y + 1
    | "ne" -> fun (x, y) -> x + 1, y
    | "se" -> fun (x, y) -> x + 1, y - 1
    | "s" -> fun (x, y) -> x, y - 1
    | "sw" -> fun (x, y) -> x - 1, y
    | "nw" -> fun (x, y) -> x - 1, y + 1
    | x -> failwithf "Unrecognized movement '%s'" x

let input = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") |> Array.head
let split (str:string) = 
    str.Split([|','|], StringSplitOptions.RemoveEmptyEntries)

let data = 
    input 
    |> split
    |> Array.map parseMovement
    |> List.ofArray

let distance x y =
    let z : int = - x - y
    [x; y; z] |> List.map Math.Abs |> List.max    

let x, y = data |> List.fold (fun (x, y) i -> i (x, y)) (0, 0)

let result1 = distance x y
printfn "Solution 1: %i" result1

let foldWithMax (x, y, max) movement =
    let x', y' = movement (x, y)
    let d' = distance x' y'
    let max' = if (d' > max) then d' else max;
    (x', y', max')

let _, _, result2 = data |> List.fold foldWithMax (0, 0, 0)
printfn "Solution 2: %i" result2
