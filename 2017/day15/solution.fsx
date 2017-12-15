#!/usr/local/bin/fsharpi
open System

// INPUT
// Generator A starts with 722
// Generator B starts with 354

let startValueA = 722L 
let startValueB = 354L 
let factorA = 16807L
let factorB = 48271L
let divider = 2147483647L


let generatorBasic divider factor startValue = 
    seq {
        let mutable currentValue = startValue
        let nextValue factor previous = (previous * factor) % divider
        while true do
            currentValue <- nextValue factor currentValue
            yield currentValue
    }
let generator = generatorBasic divider

let areFirst16get1or0 (x:int64, y:int64) =
    let xb = Convert.ToString(x, 2).PadLeft(32, '0').Substring(16)
    let yb = Convert.ToString(y, 2).PadLeft(32, '0').Substring(16)
    if xb = yb then 1 else 0

let generatorA = generator factorA startValueA 
let generatorB = generator factorB startValueB 
let mainSeq = Seq.zip generatorA generatorB |> Seq.map areFirst16get1or0

let result1 = mainSeq |> Seq.take (40 * 1000 * 1000) |> Seq.sum
printfn "Solution 1: %i" result1

let generatorA2 = generatorA |> Seq.filter(fun x -> (x % 4L) = 0L)
let generatorB2 = generatorB |> Seq.filter(fun x -> (x % 8L) = 0L)
let mainSeq2 = Seq.zip generatorA2 generatorB2 |> Seq.map areFirst16get1or0

let result2 = mainSeq2 |> Seq.take (5 * 1000 * 1000) |> Seq.sum
printfn "Solution 2: %i" result2
