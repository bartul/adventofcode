#!/usr/local/bin/fsharpi
open System

let input = 265149


type Direction = Up | Down | Left | Right
type Point = Point of x : int * y : int
type Vector =  Vector of Direction * Point
type Position = { Vector : Vector; Value : int}

let getNextVector currentVector =
    let (Vector (direction, Point (x, y))) = currentVector
    match direction, Math.Abs(x) = Math.Abs(y) with
    | Up, true -> Vector (Left, Point (x - 1, y))
    | Up, false -> Vector (Up, Point (x, y + 1))
    | Down, true -> Vector (Right, Point (x + 1, y))
    | Down, false -> Vector (Down, Point (x, y - 1))
    | Left, true -> Vector (Down, Point (x, y - 1))
    | Left, false -> Vector (Left, Point (x - 1, y))
    | Right, true -> Vector (Up, Point (x + 1, y))
    | Right, false -> Vector (Right, Point (x + 1, y))

let folder state item = { Vector = getNextVector (List.head state).Vector; Value = item} :: state

let startPosition = { Vector = Vector (Right, Point (0, 0)); Value = 1};
let map = [2 .. input] |> List.fold folder [startPosition]

let (Vector (_, Point (x, y))) = (List.head map).Vector
printfn "Solution 1: %i" (Math.Abs(x) + Math.Abs(y))

let getNextValue currentMap (Point(x, y)) =
    currentMap 
    |> List.filter (fun { Vector = (Vector (_, Point (xi, yi)))} -> (xi = x - 1 || xi = x + 1 || xi = x) && (yi = y - 1 || yi = y + 1 || yi = y)) 
    |> List.sumBy (fun { Value = value } -> value) 

let folder2 state _ = 
    let currentPosition = List.head state
    let vector = getNextVector currentPosition.Vector
    let (Vector (_, nextPoint)) = vector
    if currentPosition.Value < input then
        let nextValue = getNextValue state nextPoint
        { Vector = vector; Value = nextValue} :: state
    else 
        { Vector = vector; Value = currentPosition.Value} :: state
        
let map2 = [2 .. input] |> List.fold folder2 [startPosition]

printfn "Solution 2: %i" (List.head map2).Value

