namespace Regressif

open Regressif
open MathNet.Numerics.LinearAlgebra

module State =
    let init () =
        { Columns =
            [ { Name = ColumnName "a"
                MatrixIndex = 0
                Type = ColumnType.Values }
              { Name = ColumnName "b"
                MatrixIndex = 1
                Type = ColumnType.Values } ]
          Matrix = DenseMatrix.init 4 2 (fun i j -> i)
          MatrixLastGenerationId = 0u
          ColumnsToPlot = ColumnName "a", ColumnName "b"
        }

    let updateMatrix model matrixManipMsg =
        let matrix = model.Matrix

        match matrixManipMsg with
        | RawMatrixManipMsg.AddRow -> matrix.InsertRow(matrix.RowCount, matrix.ColumnCount |> Array.zeroCreate |> Vector.Build.Dense)
        | RawMatrixManipMsg.RemoveRow rowIdx -> matrix.RemoveRow(rowIdx)
        | RawMatrixManipMsg.AddColumn columnInfo -> matrix.InsertColumn(matrix.ColumnCount, matrix.RowCount |> Array.zeroCreate |> Vector.Build.Dense)
        | RawMatrixManipMsg.RemoveColumn columnIdx -> matrix.RemoveColumn(columnIdx)
        |> fun matrix -> { model with Matrix = matrix }

    let update msg model =
        printfn "%A" msg

        match msg with
        | Msg.RawMatrixManip matrixManipMsg -> updateMatrix model matrixManipMsg
        | Msg.CellEdited (columnIdx, rowIdx, value) ->
            let matrix = model.Matrix
            let newValue = float value // TODO: Implement Jace.Net
            matrix.[rowIdx, columnIdx] <- newValue
            { model with
                Matrix = matrix
                MatrixLastGenerationId = model.MatrixLastGenerationId + 1u }