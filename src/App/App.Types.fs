namespace Regressif

open System

[<RequireQualifiedAccess>]
type ColumnType =
    | Values
    | Formula of string

type ColumnId =
    private | ColumnId of Guid
    static member create() = Guid.NewGuid() |> ColumnId

type Column =
    { Id: ColumnId
      Name: string
      /// The index of the column in the matrix
      MatrixIndex: int
      Type: ColumnType }

[<RequireQualifiedAccess>]
type ColumnCreationInfo =
    | Values
    | Formula of formula: string

[<RequireQualifiedAccess>]
type RawMatrixManipMsg =
    | AddRow
    | RemoveRow of rowIdx: int
    | AddColumn of ColumnCreationInfo
    | RemoveColumn of columnIdx: int
    | ReplaceValue of columnIdx: int * rowIdx: int * float

[<RequireQualifiedAccess>]
type RegressionType =
    | Linear

[<RequireQualifiedAccess>]
type Regression =
    | Linear of a: float * b: float

    static member getRegressionType reg =
        match reg with
        | Linear _ -> RegressionType.Linear


[<RequireQualifiedAccess>]
type Msg =
    // Matrix
    | CellEdited of columnIdx: int * rowIdx: int * string
    | RawMatrixManip of RawMatrixManipMsg

    // Plot
    | PlotAttached of ScottPlot.Avalonia.AvaPlot
    | ChangePlotAxis of isXAxis: bool * ColumnId
    | AutoScalePlot

    // Columns nad rows // TODO: Test all this messages
    | AddColumn of ColumnCreationInfo
    | RemoveColumn of ColumnId
    | RenameColumn of ColumnId * string
    | AddRow
    | RemoveRow of int

    // Regression
    | RegressionTypeChanged of RegressionType option
    | SetRegression of Regression option

type Model =
    { Plot: ScottPlot.Avalonia.AvaPlot option
      ColumnsToPlot: Column * Column

      Regression: Regression option

      Columns: Dictionary<ColumnId, Column>
      Matrix: float array2d
      CalculationEngine: Jace.CalculationEngine
      /// Needed for Avalonia.FuncUI to know when to update the view
      MatrixLastGenerationId: uint }
