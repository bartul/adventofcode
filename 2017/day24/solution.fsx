open System

let split (c:string) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

type Port = { Pin1: int; Pin2: int }
type Combination = { 
    Bridge: (Port * Orientation) list
    PortsLeft: Port list 
}
and Orientation = Regular | Reversed
let parsePort str =
    let parts = split "/" str |> List.map int |> List.toArray
    { Pin1 = parts.[0]; Pin2 = parts.[1]  }
let findMatch pin list =
    list 
    |> List.filter (fun x -> x.Pin1 = pin || x.Pin2 = pin)
    |> List.map (fun x -> if x.Pin1 = pin then (x, Regular) else (x, Reversed))
let openPin bridge = 
    let lastPort, lastOrientation = bridge |> List.last
    match lastOrientation with
    | Regular -> lastPort.Pin2
    | Reversed -> lastPort.Pin1
let initialPort = findMatch 0  

let addToBridge combination (port, orientation) = 
    { combination with Bridge = combination.Bridge @ [(port, orientation)]; PortsLeft = combination.PortsLeft |> List.filter (fun z -> z <> port) }
let rec combineRec finalCombinations pendingCombinations =
    let newPending =
        pendingCombinations
        |> List.fold (fun pend current -> 
            let pin = openPin current.Bridge
            let matches = current.PortsLeft |> findMatch pin
            match matches with 
            | [] -> pend
            | _ -> (matches |> List.map (addToBridge current)) @ pend
        ) []
    if newPending.IsEmpty then
        pendingCombinations @ finalCombinations
    else 
        combineRec (pendingCombinations @ finalCombinations) newPending
let combine = combineRec []    

let bridgeStrength combination =
    combination.Bridge |> List.sumBy (fun (y, _) -> y.Pin1 + y.Pin2)
let bridgeLength combination =
    combination.Bridge |> List.length
let input = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") |> Array.toList
let ports = input |> List.map parsePort
let startCombinations = ports |> initialPort |> List.map (addToBridge { Bridge = []; PortsLeft = ports })
let allCombinations = combine startCombinations 
let result1 = allCombinations |> List.map bridgeStrength |> List.max
printfn "Solution 1: %i" result1

let _, result2 = 
    allCombinations 
    |> List.map (fun x -> bridgeLength x, bridgeStrength x) 
    |> List.groupBy (fun (l, _) -> l)
    |> List.maxBy (fun (k, _) -> k)
    |> snd
    |> List.maxBy (fun (_, s) -> s)
printfn "Solution 2: %i" result2

