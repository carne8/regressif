namespace Regressif

open MathNet.Numerics.LinearAlgebra

[<RequireQualifiedAccess>]
type ColumnType =
    | Values
    | Formula of string

type ColumnName =
    | ColumnName of string
    static member raw (ColumnName value) = value

type Column =
    { /// The index of the column in the matrix
      MatrixIndex: int
      Type: ColumnType }

[<RequireQualifiedAccess>]
type RawMatrixManipMsg =
    | AddRow
    | RemoveRow of rowIdx: int
    | AddColumn of Column
    | RemoveColumn of columnIdx: int
    | ReplaceValue of columnIdx: int * rowIdx: int * float

[<RequireQualifiedAccess>]
type Msg =
    // Matrix
    | CellEdited of columnIdx: int * rowIdx: int * string
    | RawMatrixManip of RawMatrixManipMsg

    // Plot
    | PlotAttached of ScottPlot.Avalonia.AvaPlot
    | ChangePlotAxis of isXAxis: bool * ColumnName
    | AutoScalePlot

type Model =
    { Plot: ScottPlot.Avalonia.AvaPlot option
      ColumnsToPlot: ColumnName * ColumnName

      Columns: Dictionary<ColumnName, Column>
      Matrix: Matrix<float>
      CalculationEngine: Jace.CalculationEngine
      /// Needed for Avalonia.FuncUI to know when to update the view
      MatrixLastGenerationId: uint }