module Grid

// aoc15:18
type Grid<'a when 'a : equality>(jagged: 'a[][]) =

    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    let mutable formatItem = (fun x -> x.ToString())
    let nl = System.Environment.NewLine

    member _.LastCol = maxX
    member _.Width = maxX + 1
    member _.LastRow = maxY
    member _.Height = maxY + 1

    static member Generate<'a when 'a : equality>
        (width, height, gen: int -> int -> 'a) =
            [| for y in 0 .. (height - 1) do
                [| for x in 0 .. (width - 1) do
                    gen x y |] |]
            |> Grid<'a>

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.FormatItem with get() = formatItem and set(f) = formatItem <- f
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member _.Column with get(x) = data |> Array.map (fun arr -> arr.[x])
    member this.FormatRow = Array.map this.FormatItem >> (String.concat "")
    member this.AsText(y) = this.FormatRow (this.Row(y))

    member this.FormatGrid = Array.map this.FormatRow >> (String.concat nl)
    member this.AsText() = this.FormatGrid data
    override this.ToString() = this.AsText()
    member this.Display() = printfn "%s" (this.AsText())
    member this.TeeDisplay() = this.Display(); this

    member _.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Copy() = this.Transform (fun g x y -> g.[x,y])

    member this.Flatten() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield ((x, y), data.[y].[x]) }

    member _.Coords() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield (x, y) }

    member this.Filter(pred) = this.Flatten() |> Seq.filter (snd >> pred)

    member this.Find(pred) = this.Filter(pred) |> Seq.head |> fst



    member this.NHoodCoords(x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do (x, y) |]

    member this.NHood(x, y) =
        this.NHoodCoords (x, y)
        |> Array.map this.TryGet

    member this.AdjacentCoords(x, y) =
        let nhood = this.NHoodCoords (x, y)
        Array.append nhood.[0 .. 3] nhood.[5 .. 8]

    member this.Adjacent(x, y) =
        this.AdjacentCoords (x, y)
        |> Array.map this.TryGet

    member this.BorderingCoords (x, y) =
        [| (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y); |]

    member this.Bordering(x, y) =
         this.BorderingCoords (x, y)
         |> Array.map this.TryGet

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. maxY do
                [| for x in 0 .. maxX do
                    generate this x y |] |]
            |> Grid<'b>

    member _.Crop(x, width, y, height) =
        data.[y .. (y + height - 1)]
        |> Array.map (fun row -> row.[x .. (x + width - 1)])
        |> Grid<'a>

    member _.Corners() = [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]
    member this.Get (x, y) = this.[x, y]
    member this.Set (x, y) value = this.[x, y] <- value
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

// type Thingy = Grid<char>
// let thingy =
//     lines
//     |> Array.map (fun s -> s.ToCharArray())
//     |> Thingy