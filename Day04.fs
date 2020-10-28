module Grotto

open System

// #nowarn "25"

let text = IO.File.ReadAllText("Day04.txt")
let lines = Text.RegularExpressions.Regex.Split(text, "\n(?=.)")

let toHex = (BitConverter.ToString
            >> (fun str ->str.Replace("-", String.Empty)))

let md5Inst = System.Security.Cryptography.MD5.Create()

let md5 (str :string) = 
    str
    |> Text.Encoding.ASCII.GetBytes
    |> md5Inst.ComputeHash
    |> toHex

let key = text

let nhash n =
    md5 (key + (string n))

let lzcount (str :string) =
    str.ToCharArray()
    |> Array.takeWhile ((=) '0')
    |> Array.length

let search key nlz =
    Seq.initInfinite id
    |> Seq.map (fun i -> (i, (i |> nhash |> lzcount)))
    |> Seq.find (snd >> ((<=) nlz))
    |> fst


[<EntryPoint>]
let main argv =
    search key 5
    |> printfn "Part 1 %A"

    search key 6
    |> printfn "Part 2 %A"
    0
