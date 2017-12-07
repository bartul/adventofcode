#!/usr/local/bin/fsharpi

open System
open System.IO

type Tower = { 
    Name : string
    Weight : int
    Children : string []
}
let splitFromChildren (str:string) =
    str.Split([|"->"|], StringSplitOptions.RemoveEmptyEntries)
let splitChildren (str:string) =
    str.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.Trim())
let splitNameAndWeight (str:string) =
    let parts = str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let weight = parts.[1] |> Seq.filter (fun x -> x <> '(' && x <> ')') |> String.Concat |> int
    parts.[0], weight
    

let parseTower (value : string []) =
    let children =
        if value.Length < 2 then 
            Array.empty
        else
            splitChildren value.[1]
    let name, weight = splitNameAndWeight value.[0]
    { Name = name; Weight = weight; Children = children }                

let loadData () =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt")
    |> Array.map splitFromChildren
    |> Array.filter (fun x -> x.Length <= 2)
    |> Array.map parseTower

let rec goUp (towers : Tower []) (current : Tower) =
    let parent =
        towers
        |> Array.filter (fun x -> x.Children |> Array.contains current.Name)
    match parent with
    | [||] -> current
    | [|x|] -> goUp towers x
    | _ -> failwith "Found multiple parents"

let towers = loadData()
let root = goUp towers towers.[0]

printfn "Solution 1: %s" root.Name

let getTower towers name = towers |> Array.filter (fun x -> x.Name = name) |> Array.head

let rec getTotalWeight (towers : Tower []) (current : Tower) =
    let getTower = getTower towers

    if Array.isEmpty current.Children then  
        current.Weight
    else 
        let sum = current.Children |> Array.sumBy (fun x -> getTotalWeight towers (getTower x))
        sum + current.Weight

let findParent towers item =
    towers
    |> Array.filter (fun x -> x.Children |> Array.contains item.Name)
    |> Array.tryHead
let childWeights towers item = 
    item.Children 
    |> Array.map ((getTower towers) >> (fun x -> (getTotalWeight towers x), x) )

let getDisbalancedAmongSiblings (siblingTotalWeights : (int * Tower) []) = 
    siblingTotalWeights 
        |> Array.groupBy (fun (w, _) ->  w) 
        |> Array.filter (fun (_, c) -> c.Length = 1)
        |> Array.map (fun (_, c) -> c.[0])
        |> Array.tryHead

let rec findDisbalance towers current =
    let childWeights = childWeights towers current
    match getDisbalancedAmongSiblings childWeights with 
    | Some (_, t) -> findDisbalance towers t
    | None -> current
    
let disbalance = findDisbalance towers root
let disbalanceTotalWeight = getTotalWeight towers disbalance 

let siblingsWithTotalWeights = disbalance |> findParent towers |> Option.get |> childWeights towers
let siblingsTotalWeight = siblingsWithTotalWeights |> Array.filter (fun (_, i) -> i.Name <> disbalance.Name) |> Array.map (fun (t, _) -> t) |> Array.head

let result2 = disbalance.Weight + (siblingsTotalWeight - disbalanceTotalWeight)

printfn "Solution 2: %i" result2
