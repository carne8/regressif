namespace Regressif

open Regressif
open Elmish

module Cmds =
    open MathNet.Numerics

    let private getPoints model =
        (model.ColumnsToPlot |> fst |> _.MatrixIndex |> Array2D.getColumn model.Matrix), // First column
        (model.ColumnsToPlot |> snd |> _.MatrixIndex |> Array2D.getColumn model.Matrix)  // Second column

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

                    let variableName = column.Name
                    let value =
                        model.Matrix
                        |> Array2D.at column.MatrixIndex rowIdx

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
        open ScottPlot

        let private autoScalePlot model (plot: Avalonia.AvaPlot) =
            let points = model |> getPoints
            let margin = 1.

            let xMax = points |> fst |> Array.max |> (+) margin
            let yMax = points |> snd |> Array.max |> (+) margin

            plot.Plot.Axes.SetLimits(0, xMax, 0, yMax)
            plot.Refresh()

        let autoScale model (plot: Avalonia.AvaPlot) = Cmd.ofEffect (fun _ -> autoScalePlot model plot)

        let applyPoints model =
            Cmd.ofEffect (fun _ ->
                let plot =
                    match model.Plot with
                    | None -> failwith "Plot not attached"
                    | Some plot -> plot

                let points = getPoints model

                plot.Plot.Clear()

                points
                ||> Seq.map2 (fun x y -> Coordinates(x, y))
                |> Seq.toArray
                |> plot.Plot.Add.ScatterPoints
                |> ignore

                autoScalePlot model plot
            )

        let computeRegression model regressionType =
            Cmd.ofEffect (fun dispatch ->
                let plot: Avalonia.AvaPlot =
                    match model.Plot with
                    | None -> failwith "Plot not attached"
                    | Some plot -> plot

                match regressionType with
                | None ->
                    plot.Plot.PlottableList
                    |> Seq.tryFind (fun plottable ->
                        match plottable with
                        | :? Plottables.FunctionPlot ->
                            plot.Plot.Remove plottable
                            true
                        | _ -> false
                    )
                    |> ignore

                    None

                | Some RegressionType.Linear ->
                    let struct (b, a) =
                        model
                        |> getPoints
                        |> Fit.Line

                    // Apply regression
                    fun x -> a*x + b
                    |> plot.Plot.Add.Function
                    |> ignore

                    Regression.Linear(a, b) |> Some

                |> Msg.SetRegression
                |> dispatch
            )

        let recomputeRegression model =
            match model.Regression with
            | None -> Cmd.none
            | Some regression -> computeRegression model (regression |> Regression.getRegressionType |> Some)

module State =
    let init () =
        let columns =
            [ let colId1 = ColumnId.create()
              colId1, { Id = colId1; Name = "x"; MatrixIndex = 0; Type = ColumnType.Values }

              let colId2 = ColumnId.create()
              colId2, { Id = colId2; Name = "y"; MatrixIndex = 1; Type = ColumnType.Values } ]

        { Plot = None
          ColumnsToPlot =
            columns |> List.item 0 |> snd,
            columns |> List.item 1 |> snd

          Regression = None

          Columns = columns |> dict
          Matrix = Array2D.init 4 2 (fun i j -> i + j |> float)
          CalculationEngine = Jace.CalculationEngine()
          MatrixLastGenerationId = 0u },
        Cmd.none

    let updateMatrix model matrixManipMsg =
        let matrix = model.Matrix

        match matrixManipMsg with
        | RawMatrixManipMsg.AddRow ->
            let row =
                matrix
                |> Array2D.columnCount
                |> Array.zeroCreate

            matrix |> Array2D.addRow row

        | RawMatrixManipMsg.AddColumn columnCreationInfo ->
            let columnValues =
                match columnCreationInfo with
                | ColumnCreationInfo.Values ->
                    matrix
                    |> Array2D.rowCount
                    |> Array.zeroCreate
                | ColumnCreationInfo.Formula formula ->
                    printfn "Not implemented" // TODO
                    matrix
                    |> Array2D.rowCount
                    |> Array.zeroCreate

            matrix |> Array2D.addColumn columnValues

        | RawMatrixManipMsg.RemoveRow rowIdx -> matrix |> Array2D.removeRow rowIdx
        | RawMatrixManipMsg.RemoveColumn columnIdx -> matrix |> Array2D.removeColumn columnIdx

        | RawMatrixManipMsg.ReplaceValue (columnIdx, rowIdx, value) ->
            matrix[rowIdx, columnIdx] <- value
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

            newModel,
            Cmd.batch [
                Cmds.Plot.applyPoints newModel
                Cmds.Plot.recomputeRegression newModel
            ]

        | Msg.CellEdited (columnIdx, rowIdx, value) ->
            model, Cmds.computeExpression value (rowIdx, columnIdx) model


        // Plot
        | Msg.PlotAttached plot ->
            let newModel = { model with Plot = Some plot }
            newModel,
            Cmd.batch [
                Cmds.Plot.autoScale newModel plot
                Cmds.Plot.applyPoints newModel
                Cmds.Plot.recomputeRegression newModel
            ]

        | Msg.ChangePlotAxis (isXAxis, newColumnId) ->
            let newColumnOpt = model.Columns |> Dictionary.tryGetItem newColumnId

            match newColumnOpt with
            | None -> model, Cmd.none // TODO: Find a way to show an error message
            | Some newColumn ->

                let columnsToPlot =
                    match isXAxis with
                    | true -> newColumn, model.ColumnsToPlot |> snd
                    | false -> model.ColumnsToPlot |> fst, newColumn

                let newModel = { model with ColumnsToPlot = columnsToPlot }
                newModel,
                Cmd.batch [
                    Cmds.Plot.applyPoints newModel
                    Cmds.Plot.recomputeRegression newModel
                ]

        | Msg.AutoScalePlot ->
            model,
            match model.Plot with
            | None -> Cmd.none
            | Some plot -> Cmds.Plot.autoScale model plot


        // Columns and rows
        | Msg.AddRow ->
            model,
            RawMatrixManipMsg.AddRow
            |> Msg.RawMatrixManip
            |> Cmd.ofMsg

        | Msg.RemoveRow rowIdx ->
            model,
            rowIdx
            |> RawMatrixManipMsg.RemoveRow
            |> Msg.RawMatrixManip
            |> Cmd.ofMsg

        | Msg.AddColumn columnCreationInfo ->
            let newColumn =
                match columnCreationInfo with
                | ColumnCreationInfo.Values ->
                    { Id = ColumnId.create()
                      Name = "New column" // TODO: Replace with alphabet letters
                      MatrixIndex = model.Matrix |> Array2D.columnCount
                      Type = ColumnType.Values }
                | ColumnCreationInfo.Formula formula ->
                    { Id = ColumnId.create()
                      Name = "New column"
                      MatrixIndex = model.Matrix |> Array2D.columnCount
                      Type = ColumnType.Formula formula }

            let newColumns =
                model.Columns |> Dictionary.add newColumn.Id newColumn

            { model with Columns = newColumns },
            columnCreationInfo
            |> RawMatrixManipMsg.AddColumn
            |> Msg.RawMatrixManip
            |> Cmd.ofMsg

        | Msg.RemoveColumn columnId ->
            let column =
                model.Columns
                |> Dictionary.tryGetItem columnId
                |> Option.defaultWith (fun _ -> failwith "Column not found")

            let newColumns =
                model.Columns |> Dictionary.remove columnId

            { model with Columns = newColumns },
            column.MatrixIndex
            |> RawMatrixManipMsg.RemoveColumn
            |> Msg.RawMatrixManip
            |> Cmd.ofMsg

        | Msg.RenameColumn (columnId, newName) ->
            let newColumns, newColumnsToPlot =
                model.Columns |> Seq.mapFold
                    (fun columnsToPlot column ->
                        let newColumn =
                            match column.Key = columnId with
                            | true -> column.Key, { column.Value with Name = newName }
                            | false -> column.Key, column.Value

                        let newColumnsToPlot =
                            match columnsToPlot with
                            | (col, _) when col.Id = (newColumn |> fst) ->
                                newColumn |> snd, columnsToPlot |> snd
                            | (_, col) when col.Id = (newColumn |> fst) ->
                                columnsToPlot |> fst, newColumn |> snd
                            | _ -> columnsToPlot

                        newColumn, newColumnsToPlot
                    )
                    model.ColumnsToPlot

            { model with
                Columns = newColumns |> dict
                ColumnsToPlot = newColumnsToPlot },
            Cmd.none


        // Regression
        | Msg.RegressionTypeChanged regType -> model, Cmds.Plot.computeRegression model regType
        | Msg.SetRegression regression -> { model with Regression = regression }, Cmd.none

