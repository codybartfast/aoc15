open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day06.txt")
let lines = Regex.Split(text, "\n(?=.)")

type Inst =  { Action: string; From: int * int; To: int * int;}
let X, Y = fst, snd

let inst (line: string) =
    let m = Regex.Match(line, @"^([\w ]+\w) (\d+),(\d+) through (\d+),(\d+)")
    { Action = m.Groups.[1].Value
      From = (int m.Groups.[2].Value), (int m.Groups.[3].Value)
      To = (int m.Groups.[4].Value), (int m.Groups.[5].Value) }
let insts = Array.map inst lines

let lights () = Array.init 1000 (fun _ -> Array.zeroCreate<int> 1000)
let updateLight (lights :int[][]) x y fn = lights.[y].[x] <- (fn (lights.[y].[x]))
let sumLights = Array.sumBy Array.sum

let setup turnOn toggle turnOff lights =
    insts
    |> Array.iter (fun inst ->
        for x in  X inst.From .. X inst.To do
            for y in Y inst.From .. Y inst.To do
                match inst.Action with
                | "turn on" -> updateLight lights x y turnOn
                | "toggle" -> updateLight lights x y toggle
                | "turn off" -> updateLight lights x y turnOff
                | _ -> failwith "oops" )
    lights

let brightness turnOn toggle turnOff =
    lights ()
    |> setup turnOn toggle turnOff
    |> sumLights

[<EntryPoint>]
let main argv =
    brightness (fun _ -> 1) (fun n -> 1 - n) (fun _ -> 0)
    |> printfn "Part 1 %A"

    brightness ((+) 1) ((+) 2) (function 0 -> 0 | n -> n - 1)
    |> printfn "Part 2 %A"
    0
