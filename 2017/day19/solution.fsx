open System;

type Vector = {
    X : int
    Y : int
    Direction : Direction
}
and Direction = Up | Down | Right | Left
and TileType = 
    | HorizontalRoad
    | VerticalRoad
    | Crossroad
    | Blank
    | Letter of char

let parseTiles = function
    | '-' -> HorizontalRoad
    | '|' -> VerticalRoad
    | '+' -> Crossroad
    | ' ' -> Blank
    | c -> Letter c 
let nextTileInDirection x y direction  =
    match direction with
    | Up -> x - 1, y
    | Down -> x + 1, y
    | Right -> x, y + 1
    | Left -> x, y - 1
let tileAtCor x y (map: TileType list list) = 
    map.[x].[y]
let tileAt vector map =
    map |> tileAtCor vector.X vector.Y
let nonBlanks x y directions map = 
    directions 
    |> List.map ((fun d -> (nextTileInDirection x y d), d) >> (fun ((x, y), d) -> x, y, d))
    |> List.filter (fun (x, y, _) -> (map |> tileAtCor x y) <> Blank)
    |> List.tryHead

let rec moveRec letters steps vector (map: TileType list list) = 
    let nextCalc, nextLetters =
        match map |> tileAt vector with
        | HorizontalRoad | VerticalRoad ->
            let next = nonBlanks vector.X vector.Y [vector.Direction]
            next, letters
        | Letter c ->
            let next = nonBlanks vector.X vector.Y [vector.Direction]
            next, letters @ [c]
        | Crossroad ->
            let next = 
                match vector.Direction with 
                | Up | Down -> nonBlanks vector.X vector.Y [Left; Right]
                | Left | Right -> nonBlanks vector.X vector.Y [Up; Down]
            next, letters            
        | _ -> failwith "Uncharted territory"        
    match map |> nextCalc with 
    | Some (xn, yn, direction) -> 
        moveRec nextLetters (steps + 1) { vector with X = xn; Y = yn; Direction = direction } map
    | None -> 
        nextLetters, steps        
let move = moveRec [] 1

let input = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") 
    |> Array.map (Seq.toList >> List.map parseTiles)
    |> List.ofArray

let firstPosition = input.[0] |> List.findIndex (fun x -> x = VerticalRoad)
let result1, result2 = input |> move { X = 0; Y = firstPosition; Direction = Down; }

printfn "Solution 1: %s" (result1 |> List.toArray |> String)
printfn "Solution 2: %i" result2
