module Grotto

let text = System.IO.File.ReadAllText("Day03.txt")

let moves = text.ToCharArray()

let newpos (x, y) mov =
    match mov with
    | '^' -> (x + 1, y)
    | '>' -> (x, y + 1)
    | 'v' -> (x - 1, y)
    | '<' -> (x, y - 1)
    | _ -> failwith "oops"
   
let positions moves = 
    moves
    |> Array.scan newpos (0, 0)
    |> Array.distinct


[<EntryPoint>]
let main argv =
    moves
    |> positions
    |> Array.length
    |> printfn "Part 1 %A"

    let pairs = Array.chunkBySize 2 moves 
    let smoves = pairs |> Array.map Array.head
    let rmoves = pairs |> Array.map Array.last

    Array.concat
        [| (positions smoves); (positions rmoves)|]
    |> Array.distinct
    |> Array.length
    |> printfn "Part 2 %A"
    0
