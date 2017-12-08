open System
open System.IO

type Modifier<'a> = 'a -> 'a -> 'a
type Condition<'a> = 'a -> 'a -> bool
type Instruction = { 
    RegisterName : string
    Mod : Modifier<int>
    Value : int 
    RegisterToCheck : string 
    Condition : Condition<int>
    ConditionValue : int
}

let parseModifier str =
    match str with
    | "inc" -> (+)
    | "dec" -> (-)
    | _ -> failwith "Unrecognized modifier"
let parseCondition str =
    match str with
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
        RegisterToCheck = parts.[4] 
        Condition = parts.[5] |> parseCondition
        ConditionValue = parts.[6] |> int
    }

let loadData () =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.map parseInstruction

let addNewIfEmpty key value (map:Map<'a, 'b>) =
    if map.ContainsKey key then 
        map
    else  
        map.Add(key, value) 
let getGretter a b =
    if a > b then a else b

let folder (set, maxValue) instr =
    let set = 
        set 
        |> addNewIfEmpty instr.RegisterName 0 
        |> addNewIfEmpty instr.RegisterToCheck 0 
    if instr.Condition set.[instr.RegisterToCheck] instr.ConditionValue then
        let currentValue = set.[instr.RegisterName]
        let newValue = instr.Mod currentValue instr.Value
        set |> Map.remove instr.RegisterName |> Map.add instr.RegisterName newValue, getGretter maxValue newValue
    else
        set, maxValue        

let set, result2 = loadData() |> Array.fold folder (Map.empty, 0)
let result1 = set |> Map.toArray |> Array.maxBy (fun (_, x) -> x) |> snd

printfn "Solution 1: %i" result1
printfn "Solution 2: %i" result2


