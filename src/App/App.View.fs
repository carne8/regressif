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
    (columns: Dictionary<ColumnId, Column>)
    (xSelectedColumn: Column,
     ySelectedColumn: Column)
    dispatch
    =
    let selectedColumnId =
        match isXAxis with
        | true -> xSelectedColumn.Id
        | false -> ySelectedColumn.Id

    let selectedColumnIndex =
        columns
        |> Seq.tryFindIndex (_.Key >> (=) selectedColumnId)
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
                ComboBoxItem.content kv.Value.Name
                ComboBoxItem.dataContext kv.Key
            ] :> Types.IView
        )
        |> Seq.toList
        |> ComboBox.viewItems

        // Selection
        ComboBox.selectedIndex selectedColumnIndex // selectedIndex prop must be below viewItems
        ComboBox.onSelectedItemChanged (fun item ->
            let columnId =
                item
                :?> ComboBoxItem
                |> _.DataContext
                :?> ColumnId

            Msg.ChangePlotAxis (isXAxis, columnId)
            |> dispatch
        )
    ]

let plot model dispatch =
    DockPanel.create [
        DockPanel.lastChildFill true
        DockPanel.children [
            // Controls
            textButton "Fit" (fun _ -> Msg.AutoScalePlot |> dispatch)
            |> View.withAttrs [ Button.dock Dock.Top ]

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

let regressionPanel model dispatch =
    let regressionTypeComboBox () =
        let regressions =
            [ "None", None
              "Linear", Some RegressionType.Linear ]

        let selectedIndex =
            model.Regression
            |> Option.map Regression.getRegressionType
            |> fun regType -> regressions |> List.tryFindIndex (snd >> (=) regType)
            |> Option.defaultValue 0

        ComboBox.create [
            // Content
            regressions
            |> List.map (fun (name, _) ->
                ComboBoxItem.create [
                    ComboBoxItem.content name
                ] :> Types.IView
            )
            |> ComboBox.viewItems

            // Selection
            ComboBox.selectedIndex selectedIndex
            ComboBox.onSelectedIndexChanged (fun newIdx ->
                regressions
                |> List.item newIdx
                |> snd
                |> Msg.RegressionTypeChanged
                |> dispatch
            )
        ]

    StackPanel.create [
        StackPanel.minWidth 150.
        StackPanel.maxWidth 175.
        StackPanel.children [
            regressionTypeComboBox ()

            match model.Regression with
            | None -> ()
            | Some regression ->
                let constants, formula =
                    match regression with
                    | Regression.Linear (a, b) ->
                        [ "a", a; "b", b ],
                        sprintf "%s = a%s + b"
                            (model.ColumnsToPlot |> snd |> _.Name)
                            (model.ColumnsToPlot |> fst |> _.Name)

                TextBox.create [ // TODO: Use mono font
                    TextBox.text formula
                    TextBox.isReadOnly true
                ]

                yield! constants |> List.map (fun (constName, c) ->
                    TextBox.create [
                        TextBox.text (sprintf "%s = %.2f" constName c)
                        TextBox.isReadOnly true
                    ] :> Types.IView
                )
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
                    Grid.create [
                        Grid.columnDefinitions "Auto, *"
                        Grid.showGridLines true
                        Grid.children [
                            regressionPanel model dispatch
                            |> View.withAttrs [ Panel.column 0 ]

                            plot model dispatch
                            |> View.withAttrs [ Panel.column 1 ]
                        ]
                    ]
                )
            ]
        ]
    ]