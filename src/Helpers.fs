[<AutoOpen>]
module Helpers

type Dictionary<'key, 'value> = System.Collections.Generic.IDictionary<'key, 'value>

module Dictionary =
    let tryGetItem key (dict: Dictionary<_, _>) =
        match dict.TryGetValue key with
        | true, value -> Some value
        | false, _ -> None

    let tryGetTwoItems key1 key2 (dict: Dictionary<_, _>) =
        let mutable one = None
        let mutable two = None
        let mutable i = 0

        while one.IsNone || two.IsNone || i < dict.Count do
            let kv = dict |> Seq.item i
            if kv.Key = key1 then one <- Some kv.Value
            if kv.Key = key2 then two <- Some kv.Value

            i <- i + 1

        match one, two with
        | Some one, Some two -> Some (one, two)
        | _ -> None

[<RequireQualifiedAccess>]
module Vector =
    open MathNet.Numerics.LinearAlgebra
    let item index (vec: Vector<'T>) = vec.Item index

module Array2D =
    let column index (array: 'T[,]) =
        Array.init (array |> Array2D.length1) (fun i -> array.[i, index])

    let tryColumn index (array: 'T[,]) =
        match index < (array |> Array2D.length1) with
        | true -> Some (column index array)
        | false -> None

    let row index (array: 'T[,]) =
        Array.init (array |> Array2D.length2) (fun i -> array.[index, i])

    let tryRow index (array: 'T[,]) =
        match index < (array |> Array2D.length2) with
        | true -> Some (row index array)
        | false -> None

    let columns (array: 'T[,]) =
        Array.init (array |> Array2D.length2) (fun i -> column i array)

    let rows (array: 'T[,]) =
        Array.init (array |> Array2D.length1) (fun i -> row i array)
