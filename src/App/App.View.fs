module Regressif.View

open Regressif.Components
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
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

let columnSelector
    isXAxis
    (columns: Dictionary<ColumnName, Column>)
    (xSelectedColumn: ColumnName,
     ySelectedColumn: ColumnName)
    dispatch
    =
    let selectedColumnName =
        match isXAxis with
        | true -> xSelectedColumn
        | false -> ySelectedColumn

    let selectedColumnIndex =
        columns
        |> Seq.tryFindIndex (_.Key >> (=) selectedColumnName)
        |> Option.defaultValue 0

    ComboBox.create [
        // Positioning
        match isXAxis with
        | false -> // Y axis
            ComboBox.dock Dock.Top
        | true -> // X axis
            yield! [
                ComboBox.dock Dock.Bottom
                ComboBox.horizontalAlignment Layout.HorizontalAlignment.Right
            ]

        // Content
        columns
        |> Seq.map (fun kv ->
            ComboBoxItem.create [
                ComboBoxItem.content (kv.Key |> ColumnName.raw)
            ] :> Types.IView
        )
        |> Seq.toList
        |> ComboBox.viewItems

        // Selection
        ComboBox.selectedIndex selectedColumnIndex // selectedIndex prop must be below viewItems
        ComboBox.onSelectedItemChanged (fun item ->
            let columnName =
                item
                :?> ComboBoxItem
                |> _.Content
                :?> string
                |> ColumnName

            Msg.ChangePlotAxis (isXAxis, columnName)
            |> dispatch
        )
    ]

let plot model dispatch =

    DockPanel.create [
        DockPanel.dock Dock.Right
        DockPanel.lastChildFill true
        DockPanel.children [
            // Column selectors
            columnSelector false model.Columns model.ColumnsToPlot dispatch
            columnSelector true model.Columns model.ColumnsToPlot dispatch

            Plot.create [
                Plot.dock Dock.Left
                Plot.init (fun plotControl ->
                    plotControl
                    :> ScottPlot.Avalonia.AvaPlot
                    |> Msg.PlotAttached
                    |> dispatch
                )
            ]
        ]
    ]

let plotControls model dispatch =
    StackPanel.create [
        StackPanel.dock Dock.Top
        StackPanel.horizontalAlignment Layout.HorizontalAlignment.Center
        StackPanel.orientation Layout.Orientation.Horizontal
        StackPanel.children [
            textButton "Fit" (fun _ -> Msg.AutoScalePlot |> dispatch)
        ]
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
                        // DockPanel.lastChildFill true
                        DockPanel.children [
                            plotControls model dispatch
                            plot model dispatch
                        ]
                    ]
                )
            ]
        ]
    ]