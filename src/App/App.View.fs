module Regressif.View

open ScottPlot.Avalonia

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open Avalonia.Controls
open Avalonia.Data

type AvaPlot with
    static member create(attrs: IAttr<AvaPlot> list): IView<AvaPlot> =
        ViewBuilder.Create<AvaPlot>(attrs)

let view model dispatch =
    let col = model.Values.ToRowArrays()

    TabControl.create [
        TabControl.viewItems [
            TabItem.create [
                TabItem.header "Data"
                TabItem.content (
                    DataGrid.create [
                        DataGrid.items col
                        DataGrid.columns (model.Columns |> List.mapi (fun idx column ->
                            DataGridTextColumn.create [
                                DataGridTextColumn.header column.Name
                                DataGridTextColumn.binding (Binding ($"[{idx}]", BindingMode.TwoWay))
                            ]
                        ))
                    ]
                )
            ]

            TabItem.create [
                TabItem.header "Plot"
                TabItem.content (
                    AvaPlot.create [
                        AvaPlot.width 400.
                        AvaPlot.height 300.
                        AvaPlot.onInitialized (fun element ->
                            try
                                let column1 = model.Values.Column(0).AsArray()
                                let column2 = model.Values.Column(1).AsArray()

                                element.Plot.Add.Scatter(column1, column2) |> ignore
                                element.Refresh()
                            with
                            | _ -> ()

                            ()
                        )
                    ]
                )
            ]
        ]
    ]