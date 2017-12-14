#l "../day10/solution.fsx"

open System
open System.Globalization

let input = "jxqlasbh"

let ASCII key = 
    (key |> Seq.map int |> List.ofSeq) @ [17; 31; 73; 47; 23]

// Copied from day10 into single function
let hashKnot key =
    let rec calcPosition wonnabePosition array =
        let length = Array.length array
        if wonnabePosition < length then 
            wonnabePosition 
        else 
            calcPosition (wonnabePosition - length) array

    let rec cutSubList delayed position cutLength (workingArray : int []) =
        let endPosition = position + (cutLength - 1) 
        if endPosition < (Array.length workingArray) then 
            let sub = 
                workingArray 
                |> Array.indexed 
                |> Array.filter (fun (i, _) -> i >= position && i <= endPosition) 
                |> Array.map snd
                |> List.ofArray
            delayed @ sub
        else     
            let sub = workingArray |> Array.splitAt position |> snd |> List.ofArray
            cutSubList (delayed @ sub) 0 (cutLength - sub.Length) workingArray

    let rec shouldBeBetterCutSubList delayed position cutLength (workingArray : int []) =
        let newList = workingArray.[position] :: delayed
        let newCutLength = cutLength - 1
        if newCutLength = 0 then
            List.rev newList
        else
            let newPosition = workingArray |> calcPosition (position + 1)    
            shouldBeBetterCutSubList newList newPosition newCutLength workingArray 
    let rec replace position subList (array : int []) =
        match subList with
        | [] -> ()
        | [x] -> array.[position] <- x
        | head::tail -> 
            array.[position] <- head
            replace (array |> calcPosition (position + 1)) tail array
    let folder (array, currentPosition, skip) item =
        let sub = array |> cutSubList [] currentPosition item 
        let newSub = List.rev sub
        
        replace currentPosition newSub array

        let newPosition = array |> calcPosition (currentPosition + item + skip)
        let newSkip = skip + 1
        (array, newPosition, newSkip)

    let folder2 (workArray, currentPosition, currentSkip, input) _ =
        let workArray, newPosition, newSkip = input |> List.fold folder (workArray, currentPosition, currentSkip)
        workArray, newPosition, newSkip, input

    let workList = [|0..255|]
    let denseHash = List.fold (^^^) 0 
    let toHex = (sprintf "%x") >> (fun x -> if x.Length = 1 then "0" + x else x)

    let finalList, _, _, _ = [1..64] |> List.fold folder2 (workList, 0, 0, key)
    finalList |> List.ofArray |> List.chunkBySize 16 |> List.map (denseHash >> toHex) |> String.Concat

let charAsHexToInt c = 
    Int32.Parse(string c, NumberStyles.AllowHexSpecifier)  
let intToBinary (i:int) =
    Convert.ToString(i, 2).PadLeft(4, '0')
let toBinaryArray knot =
    knot 
    |> Seq.map (charAsHexToInt >> intToBinary)
    |> String.Concat
    |> Seq.map (string >> int)
    |> Array.ofSeq

let disk = 
    [0..127]
    |> List.map ((sprintf "%s-%i" input) >> ASCII >> hashKnot >> toBinaryArray)
    |> Array.ofList    

let result1 = disk |> Array.sumBy (Array.sum)
printfn "Solution 1: %i" result1

let usedNeighborsOf field (diskArray : int [] []) =
    let x, y = field
    seq {
        yield if y <> 0 && diskArray.[x].[y - 1] = 1 then Some (x, y - 1) else None
        yield if x <> 0 && diskArray.[x - 1].[y] = 1 then Some (x - 1, y) else None
        yield if y < (Array.length diskArray) - 1 && diskArray.[x].[y + 1] = 1 then Some (x, y + 1) else None
        yield if x < (Array.length diskArray) - 1 && diskArray.[x + 1].[y] = 1 then Some (x + 1, y) else None
    } |> Seq.filter Option.isSome |> Seq.map Option.get |> List.ofSeq


let rec buildRegionRec delay (diskArray : int [] []) field =
    let neighburs = usedNeighborsOf field diskArray
    let newNeighburs = neighburs |> List.filter (fun x -> delay |> (List.contains x >> not))
    if newNeighburs.Length = 0 then
        delay
    else
        let regionBuilder = buildRegionRec (newNeighburs @ delay) diskArray
        newNeighburs 
        |> List.collect regionBuilder 
        |> List.distinct

let buildRegion x diskArray = buildRegionRec [x] diskArray x


let isInRegion field (regions : (int * int) list list) =
    regions |> List.exists (List.contains field) 
let nextField length current  =
    match current with
    | x, y when x = length - 1 && y = length - 1 -> None
    | x, y when y = length - 1 -> Some (x + 1, 0)
    | x, y -> Some (x, y + 1)

let rec processDiskRec (delay : (int * int) list list) currentField (diskArray : int [] []) =
    let x, y = currentField
    let newDelay =
        if (isInRegion currentField delay) |> not && diskArray.[x].[y] = 1 then
            (diskArray |> buildRegion currentField) :: delay
        else
            delay
    match nextField (Array.length diskArray) currentField with
    | Some next -> 
        processDiskRec newDelay next diskArray
    | None -> 
        newDelay 
let processDisk = processDiskRec [] (0, 0) 

let result2 = disk |> processDisk |> List.length
printfn "Solution 2: %i" result2




