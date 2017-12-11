#!/usr/local/bin/fsharpi

open System
open System.IO

type Movement = N | NE | SE | S | SW | NW

let parseMovement = function
    | "n" -> N
    | "ne" -> NE
    | "se" -> SE
    | "s" -> S
    | "sw" -> SW
    | "nw" -> NW
    | x -> failwithf "Unrecognized movement '%s'" x

let moveNext (x, y) = function
    | N -> x, y + 1
    | NE -> x + 1, y
    | SE -> x + 1, y - 1
    | S -> x, y - 1
    | SW -> x - 1, y
    | NW -> x - 1, y + 1

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

let x, y = data |> List.fold moveNext (0, 0)

let result1 = distance x y
printfn "Solution 1: %i" result1

let foldWithMax (x, y, max) movement =
    let x', y' = moveNext (x, y) movement
    let d' = distance x' y'
    let max' = if (d' > max) then d' else max;
    (x', y', max')

let _, _, result2 = data |> List.fold foldWithMax (0, 0, 0)
printfn "Solution 2: %i" result2
