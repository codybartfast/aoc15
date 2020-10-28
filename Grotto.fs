#r "/home/me/.dotnet/sdk/3.1.100/ref/mscorlib.dll"
#r "/home/me/.dotnet/sdk/3.1.100/ref/netstandard.dll"

#load "Input.fsx"
let input = Input.input.ToCharArray() |> List.ofArray

let rec floor start chars =
    match chars with
    | [] -> start
    | '('::t -> floor (start + 1) t
    | ')'::t -> floor (start - 1) t

floor 0 input

