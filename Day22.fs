open System

let text = IO.File.ReadAllText("Day22.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let data = lines |> Array.map(fun line ->
    let fs = line.Split(": ")
    fs.[1] |> int)

type Player =
    {Mana: int; Hps: int; Arm: int;
        Shield: int; Poison: int; Recharge: int}
let player =
    {Mana = 500; Hps = 50; Arm = 0; 
    Shield = 0; Poison = 0; Recharge = 0}

type Boss = {Hps: int; Dmg: int}
let boss = {Hps = data.[0] ; Dmg = data.[1]}

type Spell = Missile | Drain | Sheild | Poison | Recharge

let cast spell (player, boss) =
    match spell with
    | Missile -> player, {boss with Boss.Hps = boss.Hps - 4}
    | Drain ->
        {player with Player.Hps = player.Hps + 2},
        {boss with Hps = boss.Hps - 2}
    | Sheild -> {player with Shield = 6 }, boss
    | Poison -> {player with Poison = 6 }, boss
    | Recharge -> {player with Recharge = 5 }, boss

let muggle (player, boss) =
    let dealt = boss.Dmg - player.Arm |> ((max) 1)
    {player with Hps = player.Hps - dealt}, boss

let effects =
    let shield (player, boss) =
        if player.Shield > 0
        then {player with Arm = 7; Shield = player.Shield - 1}, boss
        else {player with Arm = 0}, boss
    let poison (player, boss) =
        if player.Poison > 0
        then
            {player with Poison = player.Poison - 1},
            {boss with Boss.Hps = boss.Hps - 3}
        else
            player, boss
    let recharge (player, boss) =
        if player.Recharge > 0
        then
            {player with
                Mana = player.Mana + 101;
                Recharge = player.Recharge - 1},
            boss
        else
            player, boss
    shield >> poison >> recharge

let validspells player =
    let spells = []
    let spells = (Drain, 73)::spells
    let spells = if player.Recharge = 0 then (Recharge, 229)::spells else spells
    let spells = (Missile, 53)::spells
    let spells = if player.Poison = 0 then (Poison, 173)::spells else spells
    let spells = if player.Shield = 0 then (Sheild, 113)::spells else spells
    spells |> List.filter (fun sp -> (snd sp) <= player.Mana)

let check spent (player: Player, boss) = 
    if boss.Hps <= 0 then 
        Some (Seq.singleton spent)
    elif player.Hps <= 0 then 
        Some Seq.empty
    else
        None

let mutable maxspend = 1<<<11
let rec battle hard spent (player, boss) =
    let player = {player with Player.Hps = player.Hps - if hard then 1 else 0}
    match check spent (player, boss) with
    | Some r -> r 
    | None ->
    
    let player, boss = (player, boss) |> effects
    match check spent (player, boss) with
    | Some r -> r 
    | None ->

    validspells player
    |> Seq.collect (fun (spell, cost) ->
        let player, boss = (player, boss) |> (cast spell)
        let spent = spent + cost;
        if spent >= maxspend then Seq.empty else

        let player = {player with Mana = player.Mana - cost}
        match check spent (player, boss) with
        | Some r -> r 
        | None ->

        let player, boss = (player, boss) |> effects
        match check spent (player, boss) with
        | Some r -> r 
        | None ->

        let player, boss = (player, boss) |> muggle
        match check spent (player, boss) with
        | Some r -> r 
        | None ->

        battle hard spent (player, boss) )

[<EntryPoint>]
let main argv =
    battle false 0 (player, boss)
    |> Seq.min
    |> printfn "Part 2 %A"

    battle true 0 (player, boss)
    |> Seq.min
    |> printfn "Part 2 %A"
    0
