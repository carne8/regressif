namespace Regressif.Components

open ScottPlot.Avalonia
open Avalonia
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder

type Plot() =
    inherit AvaPlot()

    static member PointsProperty: StyledProperty<(float * float) array> = AvaloniaProperty.Register<Plot, _>("Points", Array.empty)
    member this.Points
        with get() = this.GetValue(Plot.PointsProperty)
        and set(value) = this.SetValue(Plot.PointsProperty, value) |> ignore


    override this.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs): unit =
        base.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs)

        match change.Property.Name with
        | "Points" ->
            this.Plot.Clear()

            change.NewValue
            |> unbox<(float * float) array>
            |> Array.map ScottPlot.Coordinates
            |> this.Plot.Add.ScatterPoints
            |> ignore

            // avaPlot.Plot.Axes.AutoScale() // TODO: Add a button to auto scale
            this.Refresh()
        | _ -> ()

    override this.OnAttachedToLogicalTree(e: LogicalTree.LogicalTreeAttachmentEventArgs) =
        base.OnAttachedToLogicalTree(e: LogicalTree.LogicalTreeAttachmentEventArgs)
        this.Plot.Axes.SquareUnits()


    // Avalonia FuncUI
    static member create attrs = ViewBuilder.Create<Plot>(attrs)
    static member points<'t when 't :> Plot> (xArr: float array, yArr: float array) : IAttr<'t> =
        let newValue =
            (xArr, yArr)
            ||> Array.map2  (fun x y -> (x, y))

        AttrBuilder<'t>.CreateProperty<(float * float) array>(Plot.PointsProperty, newValue, ValueNone)

