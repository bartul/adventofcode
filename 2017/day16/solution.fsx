open System
open System.IO

let spin steps programs =
    let length = List.length programs
    let shortSteps = steps % length
    if shortSteps <> 0 then 
        let part1, part2 = programs |> List.splitAt (length - shortSteps) 
        part2 @ part1
    else 
        programs    

let partner v1 v2 programs =
    if v1 <> v2 then
        programs 
            |> List.fold (fun state x -> (if x = v1 then v2 else if x = v2 then v1 else x) :: state) []
            |> List.rev
    else 
        programs        

let exchange i1 i2 programs =
    if i1 <> i2 then
        let v1 = programs |> List.item i1 
        let v2 = programs |> List.item i2 
        programs |> partner v1 v2
    else 
        programs        

let split (c:char) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries)
let toString (ca : char list) =
    String(ca |> List.toArray) |> string
let parseMoveAsList = function
    | 's'::tail ->
        let steps = tail |> toString |> int
        spin steps
    | 'x'::tail -> 
        let indexes = tail |> toString |> split '/' |> Array.map int
        exchange indexes.[0] indexes.[1]
    | 'p'::tail -> 
        let values = tail |> toString |> split '/' |> Array.map char
        partner values.[0] values.[1]
    | _ -> failwith "Invalid dance move"
let parseMove : string -> char list -> char list =
    Seq.toList >> parseMoveAsList


let danceMoves = 
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")
    |> split ',' 
    |> Array.map parseMove
    |> Array.toList
let programs = ['a'..'p']
 
let result1 = danceMoves |> List.fold (fun dancers move -> move dancers) programs |> toString
printfn "Solution 1: %s" result1

let rec danceRec delay programs current moves =
    let newPrograms = moves |> List.fold (fun dancers move -> move dancers) programs 
    if delay |> List.contains newPrograms then 
        (delay @ [newPrograms]), current
    else    
        danceRec (delay @ [newPrograms]) newPrograms (current + 1) moves 
let dance dancers = danceRec [dancers] dancers 1

let billion = 1000000000
let cycle, cycleLength = danceMoves |> dance programs 
let movesLeft = billion % cycleLength

let result2 = cycle.[movesLeft] 
printfn "Solution 2: %s" (result2 |> toString)
