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


let sound var map = 
    map |> resolveValue var 
let set register var map = 
    let resolvedMap, value = map |> resolveValue var 
    resolvedMap |> setValue register value
let add register var map = 
    let resolvedMap1, currentValue = map |> getValue register 
    let resolvedMap2, value = resolvedMap1 |> resolveValue var 
    resolvedMap2 |> setValue register (currentValue + value)
let multiply register var map = 
    let resolvedMap1, currentValue = map |> getValue register 
    let resolvedMap2, value = resolvedMap1 |> resolveValue var 
    resolvedMap2 |> setValue register (currentValue * value)
let modulo register var map = 
    let resolvedMap1, currentValue = map |> getValue register 
    let resolvedMap2, value = resolvedMap1 |> resolveValue var 
    resolvedMap2 |> setValue register (currentValue % value)
let recover var map = 
    let resolvedMap, value = map |> resolveValue var 
    resolvedMap, value <> 0L
let jump var1 var2 map =
    let resolvedMap1, value1 = map |> resolveValue var1 
    let resolvedMap2, value2 = resolvedMap1 |> resolveValue var2
    let jumpCommand =
        if value1 > 0L then Some value2
        else None
    resolvedMap2, jumpCommand    
let send var map = 
    map |> resolveValue var 
let receive register value map  =
    map |> setValue register value

type MutationOperation = Map<string, int64> -> Map<string, int64> 
type JumpOperation = Map<string, int64> -> Map<string, int64> * int64 option 
type SoundOperation = Map<string, int64> -> Map<string, int64> * int64 
type RecoverOperation = Map<string, int64> -> Map<string, int64> * bool
type SendOperation = Map<string, int64> -> Map<string, int64> * int64 
type ReceiveOperation = int64 -> Map<string, int64> -> Map<string, int64>

type Operation =
    | Mutation of MutationOperation    
    | Jump of JumpOperation    
    | Sound of SoundOperation    
    | Recover of RecoverOperation    

let split (c:char) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let parseOperation str =
    match (str |> split ' ') with 
    | "snd"::tail -> Sound (sound tail.[0])
    | "set"::tail -> Mutation (set tail.[0] tail.[1])
    | "add"::tail -> Mutation (add tail.[0] tail.[1])
    | "mul"::tail -> Mutation (multiply tail.[0] tail.[1])
    | "mod"::tail -> Mutation (modulo tail.[0] tail.[1])
    | "rcv"::tail -> Recover (recover tail.[0])
    | "jgz"::tail -> Jump (jump tail.[0] tail.[1])
    | _ -> failwithf "Invalid operation '%A'" str

let rec playRec currentIndex registers lastSound operations =
    match operations |> List.item currentIndex with
    | Mutation op -> 
        playRec (currentIndex + 1) (registers |> op) lastSound operations
    | Jump op -> 
        let newRegs, instruction = registers |> op
        let move = 
            match instruction with 
            | Some x -> x |> int
            | None -> 1
        playRec (currentIndex + move) newRegs lastSound operations
    | Sound op -> 
        let newRegs, sound = registers |> op
        playRec (currentIndex + 1) newRegs sound operations
    | Recover op -> 
        match registers |> op with
        | _, true -> lastSound
        | newRegs, false -> playRec (currentIndex + 1) newRegs lastSound operations
let play = playRec 0 Map.empty 0L

let input = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") |> List.ofArray

let ops = input |> List.map parseOperation 
let result1 = ops |> play
printfn "Solution 1: %i" result1

type Operation2 =
    | Mutation of MutationOperation    
    | Jump of JumpOperation    
    | Send of SendOperation    
    | Receive of ReceiveOperation

let parseOperation2 str =
    match (str |> split ' ') with 
    | "snd"::tail -> Send (send tail.[0])
    | "set"::tail -> Mutation (set tail.[0] tail.[1])
    | "add"::tail -> Mutation (add tail.[0] tail.[1])
    | "mul"::tail -> Mutation (multiply tail.[0] tail.[1])
    | "mod"::tail -> Mutation (modulo tail.[0] tail.[1])
    | "rcv"::tail -> Receive (receive tail.[0])
    | "jgz"::tail -> Jump (jump tail.[0] tail.[1])
    | _ -> failwithf "Invalid operation '%A'" str

type Process = { 
    Registers : Map<string, int64>
    State: ProcessingState
    CurrentInstructionIndex : int 
    Inbox: int64 list
    Outbox: int64 list
    OutboxCount: int
}
and ProcessingState = Awaiting | Ongoing
 

let processCurrentOperation instructions proc =
    match instructions |> List.item proc.CurrentInstructionIndex with
    | Mutation op -> 
        { proc with 
            Registers = proc.Registers |> op
            CurrentInstructionIndex = proc.CurrentInstructionIndex + 1 }
    | Jump op -> 
        let newRegs, instruction = proc.Registers |> op
        let move = 
            match instruction with 
            | Some x -> x |> int
            | None -> 1
        { proc with 
            Registers = newRegs
            CurrentInstructionIndex = proc.CurrentInstructionIndex + move }
    | Send op -> 
        let newRegs, valueToSend = proc.Registers |> op
        { proc with 
            Registers = newRegs
            CurrentInstructionIndex = proc.CurrentInstructionIndex + 1 
            Outbox = proc.Outbox @ [valueToSend] 
            OutboxCount = proc.OutboxCount + 1 }
    | Receive op -> 
        match proc.Inbox with 
        | [] -> 
            { proc with State = Awaiting }
        | head::tail -> 
            let newRegs = proc.Registers |> op head
            { proc with
                Registers = newRegs
                CurrentInstructionIndex = proc.CurrentInstructionIndex + 1 
                Inbox = tail
                State = Ongoing }


let exchangeMessages proc1 proc2 =
    { proc1 with Inbox = proc1.Inbox @ proc2.Outbox; Outbox = []}, 
    { proc2 with Inbox = proc2.Inbox @ proc1.Outbox; Outbox = []}
let isBlocked 
    p = p.State = Awaiting && p.Inbox.IsEmpty
let rec run proc1 proc2 instructionSet =

    let proc1 = proc1 |> processCurrentOperation instructionSet
    let proc2 = proc2 |> processCurrentOperation instructionSet

    let proc1, proc2 = exchangeMessages proc1 proc2

    if proc1 |> isBlocked && proc2 |> isBlocked then
        proc1, proc2
    else    
        run proc1 proc2 instructionSet

let first = { Registers = Map.empty |> Map.add "p" 0L ; State = Ongoing; CurrentInstructionIndex = 0; Inbox = []; Outbox = []; OutboxCount = 0 }
let second = { Registers = Map.empty |> Map.add "p" 1L ; State = Ongoing; CurrentInstructionIndex = 0; Inbox = []; Outbox = []; OutboxCount = 0 }
let ops2 = input |> List.map parseOperation2 
let _, result2 = ops2 |> run first second

printfn "Solution 2: %i" result2.OutboxCount
