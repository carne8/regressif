namespace Regressif.Components

open ScottPlot.Avalonia
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder

type Plot() as this =
    inherit UserControl()

    let avaPlot = AvaPlot()

    do
        avaPlot.Plot.Axes.SquareUnits()
        this.Content <- avaPlot

    static member PointsProperty: StyledProperty<(float * float) array> = AvaloniaProperty.Register<Plot, _>("Points", Array.empty)
    member this.Points
        with get() = this.GetValue(Plot.PointsProperty)
        and set(value) = this.SetValue(Plot.PointsProperty, value) |> ignore

    override _.OnPropertyChanged (change: AvaloniaPropertyChangedEventArgs): unit =
        base.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs)

        match change.Property.Name with
        | "Points" ->
            avaPlot.Plot.Clear()

            change.NewValue
            |> unbox<(float * float) array>
            |> Array.map ScottPlot.Coordinates
            |> avaPlot.Plot.Add.ScatterPoints
            |> ignore

            avaPlot.Plot.Axes.AutoScale()
            avaPlot.Refresh()
        | _ -> ()

    static member create attrs = ViewBuilder.Create<Plot>(attrs)
    static member points<'t when 't :> Plot> (xArr: float array, yArr: float array) : IAttr<'t> =
        let newValue =
            (xArr, yArr)
            ||> Array.map2  (fun x y -> (x, y))

        AttrBuilder<'t>.CreateProperty<(float * float) array>(Plot.PointsProperty, newValue, ValueNone)

