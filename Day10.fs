open System

let text = IO.File.ReadAllText("Day10.txt")
let digits = text.ToCharArray() |> List.ofArray |> List.map (string >> int)

let looknsay digits =
    let rec iter prev next digit count =
        match prev, count with
        | [], 0 -> List.rev next
        | h::t, 0 -> iter t next h 1
        | h::t, _ when h = digit -> iter t next digit (count + 1)
        | _ -> iter prev (digit::count::next) -1 0
    iter digits [] -1 0

[<EntryPoint>]
let main argv =
    [1 .. 40]
    |> List.fold (fun digits _ -> looknsay digits) digits
    |> List.length
    |> printfn "Part 1 %A"

    [1 .. 50]
    |> List.fold (fun digits _ -> looknsay digits) digits
    |> List.length
    |> printfn "Part 2 %A"
    0
