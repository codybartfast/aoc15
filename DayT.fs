open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day1.txt")
let lines = Regex.Split(text, "\n(?=.)")

[<EntryPoint>]
let main argv =
    lines
    |> printfn "Part 1 %A"

    "?"
    |> printfn "Part 2 %A"
    0
