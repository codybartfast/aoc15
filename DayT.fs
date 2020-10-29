open System
open System.Text.RegularExpressions

// #nowarn "25"

let text = System.IO.File.ReadAllText("Day0.txt")
let lines = Regex.Split(text, "\n(?=.)")

[<EntryPoint>]
let main argv =
    lines
    |> printfn "Part 1 %A"

    "?"
    |> printfn "Part 2 %A"
    0
