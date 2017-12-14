open System
open System.IO

let circularSeq offset length value =
    let cycle = (length - 1) * 2
    Seq.initInfinite (fun i -> if (i + offset) % cycle = 0 then value else 0)

let rec mergeRec delay predicate seqList =
    match seqList with 
    | head::tail -> 
        let newDelay = Seq.zip delay head |> Seq.map predicate 
        mergeRec newDelay predicate tail
    | _ -> delay    
let merge predicate seqList = 
    match seqList with
    | head::tail -> mergeRec head predicate tail
    | _ -> Seq.empty

let parseLayer valueCalc (str:string) = 
    let parts = str.Split([|':'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int
    match parts with
    | [|x; y|] -> circularSeq x y (valueCalc x y)
    | _ -> failwithf "Invalid layer '%A'" str
let input valueCalc = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") 
    |> Array.map (parseLayer valueCalc)
    |> List.ofArray


let firewallSeq1 = input (*) |> merge (fun (x, y) -> x + y) 
let result1 = firewallSeq1 |> Seq.head
printfn "Solution 1: %i" result1

let firewallSeq2 = input (fun _ _ -> 1) |> merge (fun (x, y) -> x + y) 
let result2 = firewallSeq2 |> Seq.takeWhile (fun flag -> flag <> 0) |> Seq.length 
printfn "Solution 2: %i" result2



