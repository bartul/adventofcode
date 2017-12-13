#!/usr/local/bin/fsharpi

open System
open System.IO

type Direction = Up | Down
type Layer = { Depth : int; Range : bool list; Direction : Direction }

let input = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") 

let parseLayer (str:string) = 
    let parts = 
        str.Split([|':'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    match parts with
    | [|x; y|] ->  
        let range = Array.create y false
        range.[0] <- true
        { Depth = x; Range = range |> List.ofArray; Direction = Up }
    | _ -> failwithf "Invalid layer '%A'" str

let firewall =
    input
    |> Array.map parseLayer
    |> List.ofArray

let find depth (firewall : Layer list) =
    match firewall |> List.filter (fun x -> x.Depth = depth) with 
    | [x] -> Some x
    | [] -> None
    | _ -> failwithf "Multiple layers with same depth %i" depth 

let moveRange layer  =
    let currentIndex = layer.Range |> List.findIndex id

    let newDirection, newIndex = 
        match layer.Direction, currentIndex with
        | Up, i when i = (layer.Range.Length - 1) -> Down, currentIndex - 1
        | Up, _ -> Up, currentIndex + 1
        | Down, 0 -> Up, currentIndex + 1 
        | Down, _ -> Down, currentIndex - 1 

    let newRange = Array.create layer.Range.Length false
    newRange.[newIndex] <- true

    { layer with Direction = newDirection; Range = newRange |> List.ofArray }

let moveAll firewall =
    firewall |> List.map moveRange
    
let rec severityRec delay position firewall =
    let maxDepth = firewall |> List.map (fun x -> x.Depth) |> List.max
    if position <= maxDepth then
        let currentSeverity =
            match firewall |> find position with 
            | Some currentLayer -> 
                if currentLayer.Range.[0] then currentLayer.Depth * currentLayer.Range.Length else 0
            | None ->
                0
        let newFirewall = moveAll firewall            
        severityRec (delay + currentSeverity) (position + 1) newFirewall
    else    
        delay

let severity = severityRec 0 0    

let rec canPassRec delay position (firewall : Layer list) =
    let maxDepth = firewall |> List.map (fun x -> x.Depth) |> List.max
    if position <= maxDepth then
        let currentIsOk =
            match firewall |> find position with 
            | Some currentLayer -> currentLayer.Range.[0] |> not
            | None -> true
        if currentIsOk then        
            let newFirewall = moveAll firewall            
            canPassRec currentIsOk (position + 1) newFirewall
        else
            false        
    else    
        delay
let canPass = canPassRec true 0    


let rec calcDelayRec delay firewall =
    if canPass firewall then
        delay
    else 
        let newFirewall = moveAll firewall
        let newDelay = delay + 1 
        calcDelayRec newDelay newFirewall

let calcDelay = calcDelayRec 0

let result1 = severity firewall
printfn "Solution 1: %i" result1

let result2 = calcDelay firewall
printfn "Solution 2: %i" result2

