namespace Regressif

open Regressif
open Elmish
open MathNet.Numerics.LinearAlgebra

module Cmds =
    let computeExpression expression (rowIdx, columnIdx) model =
        Cmd.ofEffect (fun dispatch ->
            // Get function from expression
            let func =
                expression
                |> model.CalculationEngine.Build
                |> _.Invoke

            // Get all columns values for this row
            let variableDict =
                model.Columns
                |> Seq.map (fun kv ->
                    let column = kv.Value

                    let variableName = kv.Key |> ColumnName.raw
                    let value =
                        column.MatrixIndex
                        |> model.Matrix.Column
                        |> Vector.item rowIdx

                    variableName, value
                )
                |> dict

            // Calculate the value
            let calculatedValue = variableDict |> func

            RawMatrixManipMsg.ReplaceValue(
                columnIdx,
                rowIdx,
                calculatedValue
            )
            |> Msg.RawMatrixManip
            |> dispatch
        )

    module Plot =
        let applyPoints model =
            Cmd.ofEffect (fun _ ->
                let plot =
                    match model.Plot with
                    | None -> failwith "Plot not attached"
                    | Some plot -> plot

                let columns =
                    model.Columns
                    |> Dictionary.tryGetTwoItems
                        (model.ColumnsToPlot |> fst)
                        (model.ColumnsToPlot |> snd)

                let points =
                    match columns with
                    | None -> failwith "Columns not found"
                    | Some columns ->
                        (columns |> fst |> _.MatrixIndex |> model.Matrix.Column), // First column
                        (columns |> snd |> _.MatrixIndex |> model.Matrix.Column)  // Second column

                plot.Plot.Clear()

                points
                ||> Seq.map2 (fun x y -> ScottPlot.Coordinates(x, y))
                |> Seq.toArray
                |> plot.Plot.Add.ScatterPoints
                |> ignore
            )

module State =
    let init () =
        let columns =
            [ ColumnName "a", { MatrixIndex = 0; Type = ColumnType.Values }
              ColumnName "b", { MatrixIndex = 1; Type = ColumnType.Values } ]

        { Plot = None
          ColumnsToPlot = ColumnName "a", ColumnName "b"

          Columns = columns |> dict
          Matrix = DenseMatrix.init 4 2 (fun i j -> i)
          CalculationEngine = Jace.CalculationEngine()
          MatrixLastGenerationId = 0u },
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

    let update msg model =
        match msg with
        // Matrix
        | Msg.RawMatrixManip matrixManipMsg ->
            let newMatrix = updateMatrix model matrixManipMsg

            let newModel =
                { model with
                    Matrix = newMatrix
                    MatrixLastGenerationId = model.MatrixLastGenerationId + 1u }

            newModel, Cmds.Plot.applyPoints newModel

        | Msg.CellEdited (columnIdx, rowIdx, value) ->
            model, Cmds.computeExpression value (rowIdx, columnIdx) model


        // Plot
        | Msg.PlotAttached plot ->
            let newModel = { model with Plot = Some plot }
            newModel, Cmds.Plot.applyPoints newModel

        | Msg.ChangePlotAxis (isXAxis, newColumnName) ->
            let columnsToPlot =
                match isXAxis with
                | true -> newColumnName, model.ColumnsToPlot |> snd
                | false -> model.ColumnsToPlot |> fst, newColumnName

            let newModel = { model with ColumnsToPlot = columnsToPlot }
            newModel, Cmds.Plot.applyPoints newModel

        | Msg.AutoScalePlot ->
            model.Plot |> Option.iter (fun plot ->
                plot.Plot.Axes.AutoScale()
                plot.Refresh()
            )
            model, Cmd.none
