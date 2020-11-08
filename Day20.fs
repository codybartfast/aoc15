open System

let text = IO.File.ReadAllText("Day20.txt")
let least = int text

let deliver (houses: int[]) nhouses mult visits elf =
    let rec iter h c =
        if h <= nhouses && c < visits then
            houses.[h] <- houses.[h] + (elf * mult)
            iter (h + elf) (c + 1)
    iter elf 0

let find nhouses mult visits =
    let houses =  (Array.zeroCreate (nhouses + 1))
    seq{ 1 .. nhouses} |> Seq.iter (deliver houses nhouses mult visits)
    
    houses
    |> Array.mapi (fun i e -> i, e)
    |> Array.filter (snd >> ((<=) least))
    |> Seq.minBy fst |> fst

[<EntryPoint>]
let main argv =
    let nhouses = 1 <<< 20
    find nhouses 10 nhouses
    |> printfn "Part 1 %A"

    find nhouses 11 50
    |> printfn "Part 2 %A"
    0
