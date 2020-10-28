module Grotto
#nowarn "25"

let text = System.IO.File.ReadAllText("Day02.txt")
let lines = System.Text.RegularExpressions.Regex.Split(text, "\n(?=.)")

let parse (line : string) =
    line.Split('x')
    |> Array.map int

let paperreq dims =
    let [| a; b; c|] = Array.sort dims
    a * b + 2 * (a * b + b * c + c * a)

let ribonreq dims =
    let [| a; b; c|] = Array.sort dims
    (a + b) * 2 + (a * b * c)

[<EntryPoint>]
let main argv =
    lines
    |> Array.sumBy (parse >> paperreq)
    |> printfn "Part 1 %A"

    lines
    |> Array.sumBy (parse >> ribonreq)
    |> printfn "Part 2 %A"
    0
