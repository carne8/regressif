module Regressif.View

open Regressif.Components
open Avalonia.Controls
open Avalonia.FuncUI.DSL

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
                    Plot.create [
                        // TODO: Add ability to choose which columns to plot (which columns to use as X and Y)
                        Plot.points (
                            model.Matrix.Column(0) |> Seq.toArray,
                            model.Matrix.Column(1) |> Seq.toArray
                        )
                    ]
                )
            ]
        ]
    ]