open System
open System.IO


let split (str:string) =
    str.Split([|'\t'; ' '|], StringSplitOptions.RemoveEmptyEntries)

let loadData () =
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")
    |> split
    |> Array.map int

let data = loadData ()  
// let data = [|0; 2; 7; 0|]

let max array =
    array
    |> Array.mapi (fun i x -> (x, i))
    |> Array.maxBy (fun (x, _) -> x) 

let rec spread size (array : int []) index value =
    let valueToAssign =
        if value < size then value
        else size
    let newValue = value - valueToAssign  
    let newIndex =
        if index = array.Length - 1 then 0
        else index + 1 

    array.[newIndex] <- array.[newIndex] + valueToAssign

    match newValue with
    | x when x > 0 -> spread size array newIndex newValue 
    | x when x = 0 -> ()
    | _ -> failwith "Failed as newValue was negative"

let spreadWithOne = spread 1

let rec reallocate (set : int[] list) =
    let current = List.head set
    let maxValue, maxIndex = max current

    let array = Array.copy current
    array.[maxIndex] <- 0
    spreadWithOne array maxIndex maxValue 

    let indexOfDuplicate = set |> List.rev |> List.tryFindIndex (fun i -> i = array)
    match indexOfDuplicate with
    | Some x -> List.length set, x
    | None -> reallocate (array :: set)
        

let stepsToLoopback, loopBeginsAtIndex = reallocate [data]

printfn "Solution 1: %i" stepsToLoopback
printfn "Solution 2: %i" (stepsToLoopback - loopBeginsAtIndex)



