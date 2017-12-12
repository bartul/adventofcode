#!/usr/local/bin/fsharpi

open System
open System.IO

let input = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") 

type Program = { Id : int; Connections : int list }

let parseProgram (str:string) =
    let parts = str.Split([|"<->"|], StringSplitOptions.RemoveEmptyEntries)
    let id = parts.[0].Trim() |> int
    let connections = parts.[1].Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> List.ofArray

    { Id = id; Connections = connections }

let data = input |> Array.map parseProgram |> List.ofArray

let getProgram id set =
    set |> List.filter (fun x -> x.Id = id) |> List.head

let rec calcGroupRec delay program set =
    let newConnections = program.Connections |> List.filter (fun x -> delay |> List.contains x |> not)
    if newConnections.Length = 0 then
        []
    else  
        let newGroup = newConnections @ delay
        let sub = newConnections |> List.collect (fun x -> calcGroupRec newGroup (getProgram x set)  set)
        sub @ newGroup |> List.distinct
let calcGroup = calcGroupRec []

let result1 = data |> calcGroup (data |> getProgram 0) |> List.length
printfn "Solution 1: %i" result1

let rec allGroupsRec delay data = 
    let newGroup = data |> calcGroup (data |> List.head) 
    let newData = data |> List.filter (fun x -> newGroup |> List.contains x.Id |> not)
    if newData.Length = 0 then
        newGroup :: delay
    else
        allGroupsRec (newGroup :: delay) newData
let allGroups = allGroupsRec []

let result2 = data |> allGroups |> List.length
printfn "Solution 2: %i" result2
