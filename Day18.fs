open System
open Grid

let text = IO.File.ReadAllText("Day18.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)

type Lights = Grid<char>
let getlights () = lines |> Array.map (fun s -> s.ToCharArray()) |> Lights

let setcorners (lights: Lights) =
    lights.Corners() |> Array.iter(fun (x,y) -> ((lights.[x,y] <- '#')))
    lights

let evolve (lights: Lights) =
    lights.Transform (fun lights x y ->
        let nadj =
            lights.Adjacent (x, y)
            |> Seq.choose id
            |> Seq.filter ((=) '#')
            |> Seq.length
        match lights.[x, y] with
        | '#' when nadj < 2 || nadj > 3 -> '.'
        | '.' when nadj = 3 -> '#'
        | l -> l )

let rec repeat n fn a = match n with 0 -> a | _ -> repeat (n - 1) fn (fn a)
let oncount (lights: Lights) = lights.Filter ((=) '#') |> Seq.length

[<EntryPoint>]
let main argv =
    repeat 100 evolve (getlights ())
    |> oncount |> printfn "Part 1 %A"

    repeat 100 (evolve >> setcorners) (getlights () |> setcorners)
    |> oncount |> printfn "Part 2 %A"
    0
