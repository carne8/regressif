[<AutoOpen>]
module Helpers

open System.Collections.Generic
type Dictionary<'key, 'value> = IDictionary<'key, 'value>

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

    let add key value (d: Dictionary<_, _>) =
        Seq.append
            d
            [ KeyValuePair(key, value) ]
        |> Dictionary<_, _>

    let remove (key: 'k) (d: Dictionary<'k, _>) =
        d
        |> Seq.filter (_.Key >> (<>) key)
        |> Dictionary<_, _>

[<RequireQualifiedAccess>]
module Vector =
    open MathNet.Numerics.LinearAlgebra
    let item index (vec: Vector<'T>) = vec.Item index

module Array2D =
    let column index (array: 'T[,]) =
        Array.init (array |> Array2D.length1) (fun i -> array[i, index])

    let tryColumn index (array: 'T[,]) =
        match index < (array |> Array2D.length1) with
        | true -> Some (column index array)
        | false -> None

    let row index (array: 'T[,]) =
        Array.init (array |> Array2D.length2) (fun i -> array[index, i])

    let tryRow index (array: 'T[,]) =
        match index < (array |> Array2D.length2) with
        | true -> Some (row index array)
        | false -> None

    let columns (array: 'T[,]) =
        Array.init (array |> Array2D.length2) (fun i -> column i array)

    let rows (array: 'T[,]) =
        Array.init (array |> Array2D.length1) (fun i -> row i array)

    let getColumn array index = column index array

    let at column row (array: 'T[,]) = array[row, column]

    let rowCount (array: 'T[,]) = array |> Array2D.length1
    let columnCount (array: 'T[,]) = array |> Array2D.length2

    let addColumn (column: 'T[]) (array: 'T[,]) =
        let rowCount = array |> rowCount
        let columnCount = array |> columnCount

        Array2D.init
            rowCount
            (columnCount + 1)
            (fun i j ->
                match j = columnCount with
                | true -> column[i]
                | false -> array[i, j]
            )

    let addRow (row: 'T[]) (array: 'T[,]) =
        let rowCount = array |> rowCount
        let columnCount = array |> columnCount

        Array2D.init
            (rowCount + 1)
            columnCount
            (fun i j ->
                match i = rowCount with
                | true -> row[j]
                | false -> array[i, j]
            )

    /// Create a new array with the specified column removed
    let removeColumn index (array: 'T[,]) =
        let rowCount = array |> rowCount
        let columnCount = array |> columnCount

        Array2D.init
            rowCount
            (columnCount - 1)
            (fun i j ->
                match j = index with
                | true -> array[i, j + 1]
                | false -> array[i, j]
            )

    /// Create a new array with the specified row removed
    let removeRow index (array: 'T[,]) =
        let rowCount = array |> rowCount
        let columnCount = array |> columnCount

        Array2D.init
            (rowCount - 1)
            columnCount
            (fun i j ->
                match i = index with
                | true -> array[i + 1, j]
                | false -> array[i, j]
            )