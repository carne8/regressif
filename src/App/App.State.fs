namespace Regressif

open Regressif
open MathNet.Numerics.LinearAlgebra

module State =
    let init () =
        { Columns =
            [ { Name = "a"
                Type = ColumnType.Values }
              { Name = "b"
                Type = ColumnType.Values } ]
          Values = DenseMatrix.init 2 2 (fun i j -> float (i + j))
        }

    let update msg model =
        match msg with
        | AddColumn columnInfo -> { model with Columns = List.append model.Columns [columnInfo] }
        | RemoveColumn columnName -> { model with Columns = model.Columns |> List.filter (fun c -> c.Name <> columnName) }
        | AddRow ->
            let values =
                model.Values.InsertRow(
                    model.Values.RowCount,
                    model.Values.ColumnCount
                    |> Array.zeroCreate
                    |> Vector.Build.Dense
                )
            { model with Values = values }
        | RemoveRow rowIdx -> { model with Values = model.Values.RemoveRow(rowIdx) }
        | EditValue (columnName, rowIdx, value) ->
            let colIdx = model.Columns |> List.findIndex (fun c -> c.Name = columnName)
            let values = model.Values.Clone()
            values.[rowIdx, colIdx] <- value
            { model with Values = values }