#!/usr/local/bin/fsharpi

open System
open System.IO

type Modifier<'a> = 'a -> 'a -> 'a
type Condition<'a> = 'a -> 'a -> bool
type Instruction = { 
    RegisterName : string
    Mod : Modifier<int>
    Value : int 
    CondRegister : string 
    Cond : Condition<int>
    CondValue : int
}

let parseModifier = function
    | "inc" -> (+)
    | "dec" -> (-)
    | _ -> failwith "Unrecognized modifier"
let parseCondition = function
    | "==" -> (=)
    | "!=" -> (<>)
    | "<" -> (<)
    | "<=" -> (<=)
    | ">" -> (>)
    | ">=" -> (>=)
    | _ -> failwith "Unrecognized condition"

let parseInstruction (str:string) =
    let parts = str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    if parts.Length > 7 then failwith "Invalid instruction"
    {
        RegisterName = parts.[0]
        Mod = parts.[1] |> parseModifier 
        Value = parts.[2] |> int
        CondRegister = parts.[4] 
        Cond = parts.[5] |> parseCondition
        CondValue = parts.[6] |> int
    }

let loadData () =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.map parseInstruction

let addNewIfMissing key value (map:Map<'a, 'b>) =
    if map.ContainsKey key then 
        map
    else  
        map.Add(key, value) 
let greater a b =
    if a > b then a else b

let folder (map, max) instr =
    let map = 
        map 
        |> addNewIfMissing instr.RegisterName 0 
        |> addNewIfMissing instr.CondRegister 0 
    if instr.Cond map.[instr.CondRegister] instr.CondValue then
        let currentValue = map.[instr.RegisterName]
        let newValue = instr.Mod currentValue instr.Value
        map |> Map.remove instr.RegisterName |> Map.add instr.RegisterName newValue, greater max newValue
    else
        map, max        

let map, result2 = loadData() |> Array.fold folder (Map.empty, 0)
let result1 = map |> Map.toArray |> Array.map snd |> Array.max

printfn "Solution 1: %i" result1
printfn "Solution 2: %i" result2


