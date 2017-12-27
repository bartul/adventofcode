open System
open System.IO

let resolveValue var map = 
    match Int64.TryParse var with 
    | true, number -> 
        map, number
    | _ -> 
        if map |> Map.containsKey var then map, map.Item var
        else (map |> Map.add var 0L), 0L
let setValue register value map =
    let cleanMap = 
        if map |> Map.containsKey register then map |> Map.remove register
        else map 
    cleanMap |> Map.add register value
let getValue register map =
    if map |> Map.containsKey register then 
        map, map.Item register
    else 
        (map |> setValue register 0L), 0L

let set register var map = 
    let resolvedMap, value = map |> resolveValue var 
    resolvedMap |> setValue register value
let subtract register var map = 
    let resolvedMap1, currentValue = map |> getValue register 
    let resolvedMap2, value = resolvedMap1 |> resolveValue var 
    resolvedMap2 |> setValue register (currentValue - value)
let multiply register var map = 
    let resolvedMap1, currentValue = map |> getValue register 
    let resolvedMap2, value = resolvedMap1 |> resolveValue var 
    resolvedMap2 |> setValue register (currentValue * value)
let jump var1 var2 map =
    let resolvedMap1, value1 = map |> resolveValue var1 
    let resolvedMap2, value2 = resolvedMap1 |> resolveValue var2
    let jumpCommand =
        if value1 <> 0L then Some value2
        else None
    resolvedMap2, jumpCommand    
    
type MutationOperation = Map<string, int64> -> Map<string, int64> 
type JumpOperation = Map<string, int64> -> Map<string, int64> * int64 option 
type Instructions =
    | Mutation of MutationOperation 
    | Multiplication of MutationOperation    
    | Jump of JumpOperation    

let split (c:char) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
let parseOperation str =
    match (str |> split ' ') with 
    | "set"::tail -> Mutation (set tail.[0] tail.[1])
    | "sub"::tail -> Mutation (subtract tail.[0] tail.[1])
    | "mul"::tail -> Multiplication (multiply tail.[0] tail.[1])
    | "jnz"::tail -> Jump (jump tail.[0] tail.[1])
    | _ -> failwithf "Invalid operation '%A'" str

let rec runRec counter currentIndex registers instructions =
    if currentIndex < (instructions |> List.length) then
        match instructions |> List.item currentIndex with
        | Mutation op -> 
            runRec counter (currentIndex + 1) (registers |> op) instructions
        | Multiplication op -> 
            runRec (counter + 1) (currentIndex + 1) (registers |> op) instructions
        | Jump op -> 
            let newRegs, instruction = registers |> op
            let move = 
                match instruction with 
                | Some x -> x |> int
                | None -> 1
            runRec counter (currentIndex + move) newRegs instructions
    else 
        counter 
let run = runRec 0 0

// let registers = 
//     ['a'..'h'] 
//     |> List.map(fun x -> x, 0) 
//     |> Map
let input = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") |> List.ofArray
let instructions = 
    input 
    |> List.map parseOperation 

let result1 = run Map.empty instructions
printfn "Solution 1: %i" result1


type Instructions2 =
    | Setting of MutationOperation * Variable * Variable
    | Subtracting of MutationOperation * Variable * Variable
    | Multiplication of MutationOperation * Variable * Variable
    | Jump of JumpOperation * Variable * Variable
and Variable = 
    | RegRef of string
    | Const of int64
let parseVariable (str:string) =
    match Int64.TryParse(str) with
    | true, x -> Const x
    | false, _ -> RegRef str  
let parseOperation2 str =
    match (str |> split ' ') with 
    | "set"::tail -> Setting (set tail.[0] tail.[1], parseVariable tail.[0], parseVariable tail.[1])
    | "sub"::tail -> Subtracting (subtract tail.[0] tail.[1], parseVariable tail.[0], parseVariable tail.[1])
    | "mul"::tail -> Multiplication (multiply tail.[0] tail.[1], parseVariable tail.[0], parseVariable tail.[1])
    | "jnz"::tail -> Jump (jump tail.[0] tail.[1], parseVariable tail.[0], parseVariable tail.[1])
    | _ -> failwithf "Invalid operation '%A'" str


let rec run2Rec currentIndex count max registers instructions =
    if count < max && currentIndex < (instructions |> List.length) then
        match instructions |> List.item currentIndex with
        | Setting (op, v1, v2) | Subtracting (op, v1, v2) | Multiplication (op, v1, v2) -> 
            match v1 with   
            | RegRef "g" -> printfn "'g' with value %A mutated by %A" (registers |> Map. .item "g") v2
            | _ -> ()
            run2Rec (currentIndex + 1) (count + 1) max (registers |> op) instructions
        | Jump (op, v1, v2) -> 
            let newRegs, instruction = registers |> op
            let move = 
                match instruction with 
                | Some x -> x |> int
                | None -> 1
            run2Rec  (currentIndex + move) (count + 1) max newRegs instructions
    else
        count
let run2 = run2Rec 0 1

let instructions2 = 
    input 
    |> List.map parseOperation2
let result2 = run2 100 ([("a", 1L)] |> Map) instructions2
// printfn "Solution 2: %i" result2
