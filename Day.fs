module Grotto

open System

// #nowarn "25"

let text = IO.File.ReadAllText("Day06.txt")
let lines = Text.RegularExpressions.Regex.Split(text, "\n(?=.)")



[<EntryPoint>]
let main argv =
    text
    |> printfn "Part 1 %A"

    "?"
    |> printfn "Part 2 %A"
    0
