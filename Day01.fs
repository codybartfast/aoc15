module Grotto
#nowarn "25"

let text = System.IO.File.ReadAllText("Day01.txt")
let chars = text.ToCharArray() |> List.ofArray
let floors = 
    List.scan
        (fun (pos, flr) chr -> 
            (pos + 1, 
                flr +  match chr with '(' -> 1 | ')' -> -1))
        (0, 0)
        chars
            

[<EntryPoint>]
let main argv =
    floors
    |> List.last
    |> snd
    |> printfn "Part1 %A" 

    floors
    |> List.where (fun (_, flr) -> flr = -1)
    |> List.head
    |> fst
    |> printfn "Part2 %A" 
    0
