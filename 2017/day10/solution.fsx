// #!/usr/local/bin/fsharpi

open System
open System.IO

let parts (str:string) = 
    str.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
let trim (str:string) = 
    str.Trim()

let input = 
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")
    |> trim

let inputAsInt = 
    input
    |> parts
    |> Array.map int
    |> List.ofArray

let inputAsASCII =
    (input |> Seq.map int |> List.ofSeq) @ [17; 31; 73; 47; 23]

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

let workList1 = [|0..255|]

let finalList, _, _ = inputAsInt |> List.fold folder (workList1, 0, 0)
let result1 = finalList.[0] * finalList.[1]
printfn "Solution 1: %i" result1

let folder2 (workArray, currentPosition, currentSkip, input) _ =
    let workArray, newPosition, newSkip = input |> List.fold folder (workArray, currentPosition, currentSkip)
    workArray, newPosition, newSkip, input


let workList2 = [|0..255|]
let finalList2, _, _, _ = [1..64] |> List.fold folder2 (workList2, 0, 0, inputAsASCII)

let denseHash = Array.fold (^^^) 0 
let toHex = Seq.map ((sprintf "%x") >> (fun x -> if x.Length = 1 then "0" + x else x)) 
    

let result2 = finalList2 |> Array.chunkBySize 16 |> Array.map denseHash |> toHex |> String.Concat
printfn "Solution 2: %s" result2


