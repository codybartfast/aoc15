module Grotto

open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day05.txt")
let lines = Text.RegularExpressions.Regex.Split(text, "\n(?=.)")

let nice (str :string) = 
    let vowels (chars :char[]) =
        chars
        |> Seq.filter (fun c -> 
            c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u')
        |> Seq.length
        |> ((<=) 3)

    let double (chars :char[]) =
        chars
        |> Seq.pairwise
        |> Seq.exists (fun (a, b) -> a = b)

    let allowed (chars :char[]) = 
         chars
        |> Seq.pairwise
        |> Seq.forall (fun pr ->
             pr <> ('a', 'b')  && 
             pr <> ('c', 'd')  && 
             pr <> ('p', 'q')  && 
             pr <> ('x', 'y'))

    let chars = str.ToCharArray()
    (vowels chars) && (double chars) && (allowed chars)

let sensible str =
    let doubdoub str =
        Regex.IsMatch(str, @"(..).*\1")

    let between str = 
        Regex.IsMatch(str, @"(.)(?!\1).\1")
    (doubdoub str) && (between str)

[<EntryPoint>]
let main argv =
    lines
    |> Seq.filter nice
    |> Seq.length
    |> printfn "Part 1 %A"

    lines
    |> Seq.filter sensible
    |> Seq.length
    |> printfn "Part 2 %A"
    0
