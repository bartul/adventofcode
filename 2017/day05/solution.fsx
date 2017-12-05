open System.IO

let loadData () =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.map int

let modifyOffset offset = offset + 1

let rec jump offsetModifier (data : int []) index stepsDone =
    let value = data.[index]
    data.[index] <- offsetModifier value
    let newIndex = index + value
    
    if newIndex < 0 || newIndex >= data.Length then
        stepsDone
    else 
        jump offsetModifier data newIndex (stepsDone + 1)

let result = jump modifyOffset (loadData()) 0 1 

printfn "Solution 1: %i" result

let modifyOffset2 offset = 
    if offset >= 3 then 
        offset - 1
    else 
        offset + 1
        
let result2 = jump modifyOffset2 (loadData()) 0 1 

printfn "Solution 2: %i" result2
