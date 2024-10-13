module Regressif.View

open Regressif.Components
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL

let textButton text onClick =
    Button.create [
        Button.onClick onClick
        Button.content (
            TextBlock.create [
                TextBlock.text text
            ]
        )
    ]

let columnSelector isXAxis (columns: Column list) dispatch =
    StackPanel.create [
        // Positioning
        match isXAxis with
        | false -> // Y axis
            StackPanel.dock Dock.Top
        | true -> // X axis
            yield! [
                StackPanel.dock Dock.Right
                StackPanel.verticalAlignment Layout.VerticalAlignment.Bottom
            ]

        StackPanel.orientation (
            match isXAxis with
            | true -> Layout.Orientation.Vertical
            | false -> Layout.Orientation.Horizontal
        )
        StackPanel.children (
            columns |> List.map (fun column ->
                textButton
                    (column.Name |> ColumnName.raw)
                    (fun _ -> Msg.ChangePlotAxis(isXAxis, column.Name) |> dispatch)
            )
        )
    ]

let view model dispatch =
    TabControl.create [
        TabControl.viewItems [
            TabItem.create [
                TabItem.header "Data"

                Spreadsheet.create [
                    Spreadsheet.init (fun el ->
                        el.OnEdited.Add(fun args ->
                            (args.ColumnIdx, args.RowIdx, args.NewText)
                            |> Msg.CellEdited
                            |> dispatch
                        )
                    )
                    Spreadsheet.data (model.Matrix.ToRowArrays())
                    Spreadsheet.columns model.Columns
                ]
                |> TabItem.content
            ]

            TabItem.create [
                TabItem.header "Plot"
                TabItem.content (
                    DockPanel.create [
                        DockPanel.lastChildFill true
                        DockPanel.children [
                            // Column selectors
                            columnSelector false model.Columns dispatch
                            columnSelector true model.Columns dispatch

                            Plot.create [
                                Plot.dock Dock.Left
                                Plot.points (model |> Model.getColumnsToPlot)
                            ]
                        ]
                    ]
                )
            ]
        ]
    ]