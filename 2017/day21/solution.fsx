open System

let split (c:string) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
type Rule = { Input : char [] []; Output : char [] []}
let parsePattern str =
    str |> split "/" |> List.toArray |> Array.map Seq.toArray
let parseRule (str:string) =
    let parts = str |> split " => "
    { 
        Input = parts |> List.item 0 |> parsePattern
        Output = parts |> List.item 1 |> parsePattern
    }    
let rotateRight (pattern:char [] []) =
    match pattern.Length with
    | 2 ->
        [|  
            [| pattern.[1].[0]; pattern.[0].[0];|];
            [| pattern.[1].[1]; pattern.[0].[1];|];
        |]
    | 3 ->
        [|  
            [| pattern.[1].[0]; pattern.[0].[0]; pattern.[0].[1];|];
            [| pattern.[2].[0]; pattern.[1].[1]; pattern.[0].[2];|];
            [| pattern.[2].[1]; pattern.[2].[2]; pattern.[1].[2];|];
        |]
    | x -> failwithf "Unsupported pattern size of '%i'" x   
let flip (pattern:char [] []) =
    match pattern.Length with
    | 2 ->
        [|  
            [| pattern.[1].[0]; pattern.[1].[1];|];
            [| pattern.[0].[0]; pattern.[0].[1];|];
        |]
    | 3 ->
        [|  
            [| pattern.[2].[0]; pattern.[2].[1]; pattern.[2].[2];|];
            [| pattern.[1].[0]; pattern.[1].[1]; pattern.[1].[2];|];
            [| pattern.[0].[0]; pattern.[0].[1]; pattern.[0].[2];|];
        |]
    | x -> failwithf "Unsupported pattern size of '%i'" x   
let doesMatch (patternA:char [] []) (patternB:char [] []) =
    if patternA.Length = patternB.Length then 
        let foldSwithed f l s = List.fold f s l
        let rotations = (patternA.Length * patternA.Length) - 1
        let rotation : char [] [] list -> char [] [] list = 
            [1..rotations] 
            |> foldSwithed (fun s _ -> (rotateRight (List.head s)) :: s)
        let all = (rotation [patternA]) @ (rotation [(flip patternA)]) |> List.distinct
        all |> List.contains patternB
    else
        false
let findMatch (ruleSet : Rule list) (pattern:char [] [])  =
    let rule = ruleSet |> List.find (fun i -> doesMatch pattern i.Input)
    rule.Output

let splitQ chunkSize totalSize =
    let spliters = [ 0..chunkSize..(totalSize - 1) ]
    List.foldBack (fun i s -> (spliters |> List.map (fun j -> i, j)) :: s) spliters []
let pointsOfQ x y size =
    let xs = [|x..(x + size)|]
    let ys = [|y..(y + size)|]
    xs |> Array.map (fun i -> ys |> Array.map (fun j -> i, j)) 
let slicePattern x y size (image : char [] []) =
    let points = pointsOfQ x y size 
    points |> Array.map (fun row -> row |> Array.map (fun (x, y) -> image.[x].[y]))
let cut (image : char [] []) =
    let cutSize = 
        if image.Length % 2 = 0 then 2
        else if image.Length % 3 = 0 then 3
        else failwithf "Unsupported size %i" image.Length
    let chunkStaringPoints = splitQ cutSize image.Length  
    chunkStaringPoints |> List.map (fun i -> i |> List.map (fun (x, y) -> slicePattern x y cutSize image))  
let stichHorizontal (imageCuts : char [] [] list) =
    let size = imageCuts |> List.head |> Array.length
    let stichLine i = 
        imageCuts 
        |> List.map (fun arr -> arr.[i])
        |> Array.concat
    [|0..(size - 1)|] |> Array.map stichLine 
let stichVertical (imageCuts : char [] [] list) =
    Array.concat imageCuts
let stich (imageCuts : char [] [] list list) =
    imageCuts |> List.map stichHorizontal |> stichVertical

let rec runRec counter ruleSet max (image : char [] []) =
    let newImage = 
        image 
        |> cut
        |> List.map (fun i -> i |> List.map (findMatch ruleSet))
        |> stich
    if counter < max then
        runRec (counter + 1) ruleSet max newImage
    else newImage    
let run = runRec 0

let startingImage = parsePattern ".#./..#/###"
let input = 
    ["../.# => ##./#../...";
    ".#./..#/### => #..#/..../..../#..#"]
    // System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") |> Array.toList
let rules = input |> List.map parseRule

// let chunkStaringPoints (image : char [] []) = splitQ 3 image.Length  

// startingImage |> chunkStaringPoints
// [[(0, 0)]] |> List.map (fun i -> i |> List.map (fun (x, y) -> slicePattern x y 1 startingImage))  

slicePattern 0 0 3 startingImage
//pointsOfQ 0 0 3
pointsOfQ 0 0 3
// let result = run rules 1 startingImage

