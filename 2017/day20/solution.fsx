open System

type Particle = {
    Name : int
    X: Vector
    Y: Vector
    Z: Vector
}
and Vector = {
    Position: int64
    Velocity: int64   
    Acceleration: int64   
} 
let split (c:string) (str:string) =
    str.Split([|c|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
let parseDimension (str:string) = 
    str.Substring(3).Replace(">", "")
    |> split ","
    |> List.map int64
let parseParticle name (str:string)  =
    let parts = str |> split ", "
    let positions = parseDimension parts.[0] 
    let velocities = parseDimension parts.[1] 
    let accelerations = parseDimension parts.[2] 
    let getVector i =    
        { Position = positions.[i]; Velocity = velocities.[i]; Acceleration = accelerations.[i] }
    { Name = name;  X = getVector 0; Y = getVector 1; Z = getVector 2 } 

let moveIt particle =
    let moveDim dimension =
        let newVelocity = dimension.Velocity + dimension.Acceleration
        let newPosition = dimension.Position + newVelocity
        { dimension with Position = newPosition; Velocity = newVelocity }
    { particle with X = moveDim particle.X; Y = moveDim particle.Y; Z = moveDim particle.Z  }    
let isExpanding particle = 
    let isDimensionExpanding dim =
        (dim.Position > 0L && dim.Velocity > 0L && dim.Acceleration >= 0L) ||
        (dim.Position < 0L && dim.Velocity < 0L && dim.Acceleration <= 0L) ||
        (dim.Velocity = 0L && dim.Acceleration = 0L)
    particle.X |> isDimensionExpanding && 
    particle.Y |> isDimensionExpanding && 
    particle.Z |> isDimensionExpanding
let IsNotExpanding particle = 
    isExpanding particle |> not
let rec moveRec particles =
    let newParticles = 
        particles 
        |> List.map moveIt
        |> List.groupBy (fun p -> p.X.Position, p.Y.Position, p.Z.Position)
        |> List.filter (fun (_, pl) -> pl.Length = 1)
        |> List.collect (fun (_, pl) -> pl)

    if newParticles |> List.exists IsNotExpanding then
        moveRec newParticles 
    else 
        newParticles    

let input = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input.txt") 
    |> List.ofArray
    |> List.mapi parseParticle

let result1 = input |> List.minBy (fun p -> Math.Abs(p.X.Acceleration) + Math.Abs(p.Y.Acceleration) + Math.Abs(p.Z.Acceleration))    
printfn "Solution 1: %i" result1.Name

let result2 = input |> moveRec |> List.length
printfn "Solution 2: %i" result2

