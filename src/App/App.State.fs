namespace Regressif

open Regressif
open Elmish
open MathNet.Numerics.LinearAlgebra

module Cmds =
    let computeExpression expression (rowIdx, columnIdx) model =
        Cmd.ofEffect (fun dispatch ->
            let func = expression |> model.CalculationEngine.Build

            // Get all columns values for this row
            let variableDict =
                model.Columns
                |> List.map (fun column ->
                    let variableName = column.Name |> ColumnName.raw
                    let value =
                        model.Matrix.Column(column.MatrixIndex)
                        |> Seq.item rowIdx

                    variableName, value
                )
                |> dict

            let calculatedValue = variableDict |> func.Invoke

            RawMatrixManipMsg.ReplaceValue(
                columnIdx,
                rowIdx,
                calculatedValue
            )
            |> Msg.RawMatrixManip
            |> dispatch
        )

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
          CalculationEngine = Jace.CalculationEngine() },
        Cmd.none

    let updateMatrix model matrixManipMsg =
        let matrix = model.Matrix

        match matrixManipMsg with
        | RawMatrixManipMsg.AddRow ->
            let row =
                matrix.ColumnCount
                |> Array.zeroCreate
                |> Vector.Build.Dense

            matrix.InsertRow(matrix.RowCount, row)

        | RawMatrixManipMsg.AddColumn columnInfo ->
            let column =
                matrix.RowCount
                |> Array.zeroCreate
                |> Vector.Build.Dense

            matrix.InsertColumn(columnInfo.MatrixIndex, column)

        | RawMatrixManipMsg.RemoveRow rowIdx -> matrix.RemoveRow(rowIdx)
        | RawMatrixManipMsg.RemoveColumn columnIdx -> matrix.RemoveColumn(columnIdx)

        | RawMatrixManipMsg.ReplaceValue (columnIdx, rowIdx, value) ->
            matrix.[rowIdx, columnIdx] <- value
            matrix

        |> fun matrix ->
            { model with
                Matrix = matrix
                MatrixLastGenerationId = model.MatrixLastGenerationId + 1u }

    let update msg model =
        match msg with
        | Msg.RawMatrixManip matrixManipMsg -> updateMatrix model matrixManipMsg, Cmd.none
        | Msg.CellEdited (columnIdx, rowIdx, value) ->
            model, Cmds.computeExpression value (rowIdx, columnIdx) model

        | Msg.ChangePlotAxis (isXAxis, newColumnName) ->
            let columnsToPlot =
                match isXAxis with
                | true -> newColumnName, model.ColumnsToPlot |> snd
                | false -> model.ColumnsToPlot |> fst, newColumnName

            { model with ColumnsToPlot = columnsToPlot }, Cmd.none