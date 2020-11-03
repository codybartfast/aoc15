open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day11.txt")
let chars = text.ToCharArray();
let last = chars.Length - 1

let incchar = function
    | 'h' -> 'j'
    | 'k' -> 'm'
    | 'n' -> 'p'
    | 'z' -> 'a'
    | c -> char ((int c) + 1)

let increment (pwd: char[]) =
    let rec iter i =
        if i < 0
        then pwd
        else
            pwd.[i] <- incchar (pwd.[i])
            if pwd.[i] = 'a'
            then iter (i - 1)
            else pwd
    iter (pwd.Length - 1)

let valid (pwd: char[]) =
    let rec run i =
        if i > last then false else
        if (int pwd.[i]) = (int pwd.[i - 1]) + 1
            && (int pwd.[i]) = (int pwd.[i - 2]) + 2
        then
            true
        else
            run (i + 1)
    let rec pairs i lstpair =
        if i > last then false else
        if (int pwd.[i]) = (int pwd.[i - 1])
        then
            match lstpair with
            | None -> pairs (i + 2) (Some pwd.[i])
            | Some prv when prv <> pwd.[i] -> true
            | _ -> pairs (i + 2) lstpair
        else
            pairs (i + 1) lstpair
    (run 2) && (pairs 1 None)

let rec search pwd =
    let next = increment pwd
    match valid next with
    | true -> next
    | false -> search next

[<EntryPoint>]
let main argv =
    search chars
    |> String
    |> printfn "Part 1 %A"

    search chars
    |> String
    |> printfn "Part 2 %A"
    0
