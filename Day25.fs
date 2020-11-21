open System

let text = IO.File.ReadAllText("Day25.txt")
let word = text.Split("., ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
let row, col = (int64 word.[15]), (int64 word.[17])

let idx col row =
    let triangle n = seq {1L..n} |> Seq.sum
    triangle (row + col - 2L) + col

let rec code seed n =
    printfn "idx: %d" n
    let next prev =(prev * 252533L) % 33554393L
    let rec iter prev n =
        match n with
        | 0L -> prev
        | _ -> iter (next prev) (n - 1L)
    iter seed (n - 1L)

let lookup col row = idx col row |> (code 20151125L)

[<EntryPoint>]
let main argv =
    lookup col row
    |> int
    |> printfn "Part 1 %A"

    "Merry Christmas"
    |> printfn "Part 2 %A"
    0
