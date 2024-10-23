namespace Regressif

open Regressif
open Elmish
open MathNet.Numerics.LinearAlgebra

module Cmds =
    open MathNet.Numerics

    let private getPoints model =
        (model.ColumnsToPlot |> fst |> _.MatrixIndex |> model.Matrix.Column), // First column
        (model.ColumnsToPlot |> snd |> _.MatrixIndex |> model.Matrix.Column)  // Second column

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
        open ScottPlot

        let init (plot: Avalonia.AvaPlot) =
            Cmd.ofEffect (fun _ ->
                // Disable all interactions
                let userInputProcessor = plot.Plot.PlotControl.UserInputProcessor
                userInputProcessor.IsEnabled <- true
                userInputProcessor.Reset()
                userInputProcessor.UserActionResponses.Clear()

                // Add right click context menu
                Interactivity.StandardMouseButtons.Right
                |> Interactivity.UserActionResponses.SingleClickContextMenu
                |> userInputProcessor.UserActionResponses.Add

                // Set menu
                plot.Menu.Clear()
                plot.Menu.Add("Save Image", fun plot ->
                    let menu = (plot.Menu :?> Avalonia.AvaPlotMenu)
                    menu.OpenSaveImageDialog(plot)
                )

                // Set integer ticks only
                let tickGenerator () =
                    TickGenerators.NumericAutomatic(
                        IntegerTicksOnly = true
                    )
                plot.Plot.Axes.Left.TickGenerator <- tickGenerator()
                plot.Plot.Axes.Bottom.TickGenerator <- tickGenerator()

                // Set grid style
                // plot.Plot.Grid.MajorLineWidth <- 0.5f // 0.5f for high resolution screens
                plot.Plot.Grid.MinorLineWidth <- 1f
                // Set grid color
                plot.Plot.Grid.MajorLineColor <- Color.Gray(80uy)
                plot.Plot.Grid.MinorLineColor <- Color.Gray(190uy)

                // Set minor grid style
                plot.Plot.Grid.XAxisStyle.MinorLineStyle.Pattern <- LinePattern.Dotted
                plot.Plot.Grid.YAxisStyle.MinorLineStyle.Pattern <- LinePattern.Dotted
            )

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
            )

        let autoScale model (plot: Avalonia.AvaPlot) =
            Cmd.ofEffect (fun _ ->
                let points = model |> getPoints
                let margin = 0.5

                let xMax = points |> fst |> Vector.max |> (+) margin
                let yMax = points |> snd |> Vector.max |> (+) margin

                plot.Plot.Axes.SetLimits(0, xMax, 0, yMax)
                plot.Refresh()
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
                    let points =
                        model
                        |> getPoints
                        |> fun (x, y) ->
                            x |> Vector.toArray,
                            y |> Vector.toArray

                    let struct (b, a) = points |> Fit.Line

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
          Matrix = DenseMatrix.init 4 2 (fun i j -> i + j |> float)
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
                Cmds.Plot.init plot
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


        // Regression
        | Msg.RegressionTypeChanged regType -> model, Cmds.Plot.computeRegression model regType
        | Msg.SetRegression regression -> { model with Regression = regression }, Cmd.none

