open System
open System.Text.RegularExpressions

#nowarn "25"

let text = IO.File.ReadAllText("Day12.txt")
let chars = text.ToCharArray() |> List.ofArray

type Val =
    | Str of string
    | Num of int
    | Arr of Val[]
    | Obj of (string * Val) list

let rec parse = function
    | '{'::cs -> object cs []
    | '"'::cs -> quoted cs []
    | '['::cs -> array cs []
    |  n::cs -> number cs [n]

and object cs kvps : char list * Val =
    let cs, Str key = quoted cs.Tail []
    let cs = cs.Tail // skip :
    let cs, value = parse cs
    let kvps = (key, value)::kvps
    match cs with
    | ','::cs -> object cs kvps
    | '}'::cs -> cs, kvps |> List.rev |> Obj

and array cs items  =
    let cs, value = parse cs
    let items = value::items
    match cs with
    | ','::cs -> array cs items
    | ']'::cs -> cs, (items |> Seq.rev |> Array.ofSeq |> Arr)

and quoted cs txt =
    match cs with
    | '"'::cs -> (cs, txt |> List.rev |> Array.ofList |> String |> Str)
    | c::cs -> quoted cs (c::txt)

and number cs digits = 
    match cs with
    | c::cs when Char.IsNumber(c) -> number cs (c::digits)
    | cs ->  cs, digits |> Seq.rev |> Array.ofSeq |> String |> int |> Num

let rec sum = function
    | Num num -> num
    | Str _ -> 0
    | Arr arr -> arr |> Array.sumBy sum
    | Obj lst -> 
        if lst |>  List.exists (function _, Str "red" -> true | _ -> false)
        then 0
        else lst |> List.sumBy (snd >> sum)

[<EntryPoint>]
let main argv =
    Regex.Matches(text, @"-?\d+")
    |> Seq.sumBy (fun m -> m.Value |> int)
    |> printfn "Part 1 %A"

    parse chars 
    |> snd
    |> sum
    |> printfn "Part 2 %A"    
    0
