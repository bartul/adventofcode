open System


let tape = [(0L, 0)] |> Map

// tape.[1L]

let write (value: int) (position: Int64) map =
    map |> Map.add position value
let moveRight (position: Int64) = position + 1L
let moveLeft (position: Int64) = position - 1L

type Operation = Map<int64, int> * int64 -> Map<int64, int> * int64 * string
type Rule = {
    State: string
} 