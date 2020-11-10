open System
open System.Text.RegularExpressions

#nowarn "25"

let text = IO.File.ReadAllText("Day23.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let insts = lines |> Array.map (fun ln ->
    match Regex.Split(ln, "[, ]+") with
    | [| a; b |] ->  a, b, None
    | [| a; b; c |] -> a, b, Some c )

let rec run (a: int64) b addr =
    let getreg reg = match reg with "a" -> a | "b" -> b
    let applyreg reg op =
        match reg with
        | "a" -> run (op a) b (addr + 1)
        | "b" -> run a (op b) (addr + 1)
    let jaddr = int >> ((+) addr)
    
    match addr < insts.Length with
    | false -> a, b
    | true -> 
        let inst, a1, a2 = insts.[addr]
        match inst with
        | "jmp" -> run a b (jaddr a1)
        | "jie" ->
            match getreg a1, a2 with
            | n, Some off when ((n % 2L) = 0L) -> run a b (jaddr off)
            | _ -> run a b (addr + 1)
        | "jio" ->
            match getreg a1, a2 with
            | 1L, Some off -> run a b (jaddr off)
            | _ -> run a b (addr + 1)
        | "inc" -> applyreg a1 ((+) 1L)
        | "tpl" -> applyreg a1 ((*) 3L)
        | "hlf" -> applyreg a1 (fun n -> n / 2L)

[<EntryPoint>]
let main argv =
    run 0L 0L 0
    |> printfn "Part 1 %A"

    run 1L 0L 0
    |> printfn "Part 2 %A"
    0
