namespace Regressif.Components

open Avalonia.FuncUI.DSL
open ScottPlot
open ScottPlot.Avalonia

type Plot() as this =
    inherit AvaPlot()

    do
        // Disable all interactions
        let userInputProcessor = this.Plot.PlotControl.UserInputProcessor
        userInputProcessor.IsEnabled <- true
        userInputProcessor.Reset()
        userInputProcessor.UserActionResponses.Clear()


        // --- Menu ---
        Interactivity.StandardMouseButtons.Right
        |> Interactivity.UserActionResponses.SingleClickContextMenu
        |> userInputProcessor.UserActionResponses.Add

        this.Menu.Clear()
        this.Menu.Add("Save Image", fun plot ->
            let menu = (plot.Menu :?> Avalonia.AvaPlotMenu)
            menu.OpenSaveImageDialog(plot)
        )

        // --- Ticks ---
        this.Plot.Axes.Left.TickGenerator <- TickGenerators.NumericAutomatic(IntegerTicksOnly = true, MinimumTickSpacing = 25f)// 50f)
        this.Plot.Axes.Bottom.TickGenerator <- TickGenerators.NumericAutomatic(IntegerTicksOnly = true, MinimumTickSpacing = 40f)// 80f)

        // --- Grid ---
        // Style
        this.Plot.Grid.MajorLineWidth <- 0.5f // 0.5f for high resolution screens
        this.Plot.Grid.MinorLineWidth <- 1f
        this.Plot.Grid.XAxisStyle.MinorLineStyle.Pattern <- LinePattern.Dotted
        this.Plot.Grid.YAxisStyle.MinorLineStyle.Pattern <- LinePattern.Dotted

        // Color
        this.Plot.Grid.MajorLineColor <- Color.Gray(80uy)
        this.Plot.Grid.MinorLineColor <- Color.Gray(190uy)

    static member create attrs = ViewBuilder.Create<Plot>(attrs)