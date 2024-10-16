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
