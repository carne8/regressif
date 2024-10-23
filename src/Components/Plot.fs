namespace Regressif.Components

open Avalonia.FuncUI.DSL
open ScottPlot.Avalonia

type Plot() =
    inherit AvaPlot()

    static member create attrs = ViewBuilder.Create<Plot>(attrs)
    // static member points<'t when 't :> Plot> (xArr: float array, yArr: float array) : IAttr<'t> =
    //     let newValue =
    //         (xArr, yArr)
    //         ||> Array.map2  (fun x y -> (x, y))

    //     AttrBuilder<'t>.CreateProperty<(float * float) array>(Plot.PointsProperty, newValue, ValueNone)
