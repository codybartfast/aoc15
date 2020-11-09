open System

let text = IO.File.ReadAllText("Day21.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let data = lines |> Array.map(fun line ->
    let fs = line.Split(": ")
    fs.[1] |> int)

let boss = (data.[0], (data.[1], data.[2]))
let playerhp = 100

let htp = fst
let eqp = snd
let dmg = snd >> fst
let arm = snd >> snd

let weapons =
    [|(8, (4, 0)); (10, (5, 0)); (25, (6, 0)); (40, (7, 0)); (74, (8, 0))|]
let armour =
    [|(0, (0, 0));
        (13, (0, 1)); (31, (0, 2)); (53, (0, 3)); (75, (0, 4)); (102, (0, 5))|]
let rings = [|(0, (0, 0)); (0, (0, 0));
                (25, (1, 0)); (50, (2, 0)); (100, (3, 0));
                (20, (0, 1)); (40, (0, 2)); (80, (0, 3))|]

let choices = seq{
    for w in 0 .. (weapons.Length - 1) do
    for a in 0 .. (armour.Length - 1) do
    for r1 in 0 .. (rings.Length - 1) do
    for r2 in r1 + 1 .. (rings.Length - 1) do
    yield
        [weapons.[w]; armour.[a]; rings.[r1]; rings.[r2]]
        |> Seq.reduce (fun (g1, (d1, a1)) (g2, (d2, a2)) ->
            (g1 + g2, ((d1 + d2), (a1 + a2)))) }


let attack attacker defender =
    let dealt = max 1 (dmg attacker - arm defender)
    ((htp defender) - dealt, eqp defender)

let rec battle player boss =
    if htp boss <= 0 then true
    elif htp player <= 0 then false
    else
    let boss = (attack player boss)
    let player = (attack boss player)
    battle player boss

let multiverse =
    let player eqp = (playerhp, eqp)
    choices
    |> Seq.map (fun (g, eqp) -> g, battle (player eqp) boss)
    |> Seq.toList

[<EntryPoint>]
let main argv =
    multiverse
    |> Seq.filter snd
    |> Seq.minBy fst |> fst
    |> printfn "Part 1 %A"

    multiverse
    |> Seq.filter (snd >> not)
    |> Seq.maxBy fst |> fst
    |> printfn "Part 2 %A"
    0
