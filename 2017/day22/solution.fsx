open System

let split (c:string) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

type Direction = North | South | West | East
type Virus = {
    X: int
    Y: int
    Direction: Direction
}
let TurnRight = function
    | North -> East
    | South -> West
    | West -> North
    | East -> South
let TurnLeft = function
    | North -> West
    | South -> East
    | West -> South
    | East -> North
let TurnBack = function
    | North -> South
    | South -> North
    | West -> East
    | East -> West
let moveToNext virus =
    match virus.Direction with 
    | North -> { virus with Y = virus.Y + 1 }
    | South -> { virus with Y = virus.Y - 1 }
    | West -> { virus with X = virus.X - 1 }
    | East -> { virus with X = virus.X + 1 }
let rec moveRec burstCounter count max virus infections =
    let isInfected x y m = 
        m |> List.exists (fun (xi, yi) -> xi = x && yi = y) 
    let nextVirus, newInfections, burstCounter =    
        if infections |> isInfected virus.X virus.Y then
            { virus with Direction = TurnRight virus.Direction },
            infections |> List.filter (fun (xi, yi) -> xi <> virus.X || yi <> virus.Y),
            burstCounter
        else        
            { virus with Direction = TurnLeft virus.Direction },
            (virus.X, virus.Y) :: infections,
            burstCounter + 1
    if count < max then
        moveRec burstCounter (count + 1) max (nextVirus |> moveToNext) newInfections        
    else 
        burstCounter, (nextVirus |> moveToNext), newInfections    
let move = moveRec 0 1

let input = 
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt") 
let length, width =
    let map = input |> split "\n" 
    map.Length, map |> List.head |> (fun x -> x.Length)
let startX, startY =
    (width / 2), - (length / 2)
let infections =
    input 
    |> split "\n"        
    |> List.mapi (fun y str -> str |> Seq.mapi (fun x char -> (x, -y, char)) |> Seq.filter (fun (_,_, c) -> c = '#') |> Seq.map(fun (x, y, _) -> x, y) |> List.ofSeq )
    |> List.concat

let result1, _, _ = move 10000 { X = startX; Y = startY; Direction = North} infections
printfn "Solution 1: %i" result1

type InfectionType = Infected | Weakened | Flagged | Clean
let turnEvolved currentDirection infectionType =
    match infectionType with 
    | Infected -> TurnRight currentDirection
    | Weakened -> currentDirection
    | Flagged -> TurnBack currentDirection
    | Clean -> TurnLeft currentDirection
let markEvolved = function
    | Infected -> Flagged
    | Weakened -> Infected
    | Flagged -> Clean
    | Clean -> Weakened

let mark x y m = 
    if m |> Map.containsKey (x, y) then
        m.[x, y]
    else 
        Clean 
let rec moveEvolvedRec burstCounter count max virus infections =
    let currentMark = mark virus.X virus.Y infections    
    let nextDirection = turnEvolved virus.Direction currentMark
    let nextMark = markEvolved currentMark
    let nextVirus = { virus with Direction = nextDirection } |> moveToNext
    let nextInfections =
        match currentMark, nextMark with
        | Clean, (Infected|Weakened|Flagged) -> infections |> Map.add (virus.X, virus.Y) nextMark
        | (Infected|Weakened|Flagged), Clean -> infections |> Map.remove (virus.X, virus.Y)
        | Clean, Clean -> infections 
        | _, _ -> infections |> Map.remove (virus.X, virus.Y) |> Map.add (virus.X, virus.Y) nextMark
    let nextBurstCounter = if nextMark = Infected then burstCounter + 1 else burstCounter

    if count < max then
        moveEvolvedRec nextBurstCounter (count + 1) max nextVirus nextInfections        
    else 
        nextBurstCounter, nextVirus, nextInfections
let moveEvolved = moveEvolvedRec 0 1

let result2, _, _ = moveEvolved 10000000 { X = startX; Y = startY; Direction = North} (infections |> List.map (fun (x, y) -> (x, y), Infected) |> Map)
printfn "Solution 2: %i" result2

