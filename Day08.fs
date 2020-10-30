open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day08.txt")
let lines = Regex.Split(text, "\r?\n(?=.)")

let unescape (str: string) =
    Regex.Replace(
        str.Substring(1, str.Length - 2),
        @"\\\\|\\""|\\x[0-9a-fA-F]{2,2}",
        (fun m -> Regex.Unescape m.Value ))

let escape (str: string)  =
    sprintf "\"%s\"" (Regex.Replace(str,  @"""|\\", (fun m -> @"\" + m.Value )))

[<EntryPoint>]
let main argv =
    lines
    |> Array.sumBy (fun s -> s.Length - (unescape s).Length)
    |> printfn "Part 1 %A"

    lines
    |> Array.sumBy (fun s -> (escape s).Length - s.Length)
    |> printfn "Part 2 %A"
    0
