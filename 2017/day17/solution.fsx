let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt") 

let stepLengts =
    input |> int

let insertAt index value list =
    let part1, part2 = list |> List.splitAt (index + 1)
    part1 @ (value :: part2) 

let stepOn currentIndex length noOfSteps =
    let steps = noOfSteps % length
    let wannabe = currentIndex + steps
    if wannabe < length then
        wannabe
    else 
        wannabe % length

let rec spinRec delay currentIndex spinNo stepsToTake maxSpin =
    let insertIndex = stepOn currentIndex (List.length delay) stepsToTake
    let newDelay = delay |> insertAt insertIndex spinNo 
    let newCurrenIndex = insertIndex + 1
    if spinNo < maxSpin then
        spinRec newDelay newCurrenIndex (spinNo + 1) stepsToTake maxSpin
    else 
        newDelay, newCurrenIndex    
let spin = spinRec [0] 0 1

let result1, lastIndex = spin stepLengts 2017
printfn "Solution 1: %i" (result1 |> Array.ofList).[lastIndex + 1]

let rec spin2Rec delay lenght currentIndex spinNo stepsToTake maxSpin =
    let jump =
        let probableJump = (lenght / stepsToTake) - 1
        if lenght > (2 * stepsToTake)
            && currentIndex < stepsToTake 
            && (probableJump * stepsToTake + currentIndex) < lenght 
            && (spinNo + probableJump) < maxSpin then 
                probableJump 
        else 
            1

    let newCurrenIndex = 
        if jump = 1 then
            (stepOn currentIndex lenght stepsToTake) + 1
        else                
             currentIndex + (stepsToTake * jump) + jump
    let newLength = lenght + jump
    let newSpinNo = spinNo + jump
    let newDelay = if newCurrenIndex = 1 then spinNo else delay
    if spinNo < maxSpin then
        spin2Rec newDelay newLength newCurrenIndex newSpinNo stepsToTake maxSpin
    else 
        newDelay
let spin2 = spin2Rec 0 1 0 1

let result2 = spin2 stepLengts 50000000
printfn "Solution 2: %i" result2


