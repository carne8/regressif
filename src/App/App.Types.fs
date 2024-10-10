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
    { Name: ColumnName
      /// The index of the column in the matrix
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
    | CellEdited of columnIdx: int * rowIdx: int * string
    | RawMatrixManip of RawMatrixManipMsg

type Model =
    { Columns: Column list
      Matrix: Matrix<float>
      MatrixLastGenerationId: uint
      ColumnsToPlot: ColumnName * ColumnName
      CalculationEngine: Jace.CalculationEngine }

    static member private getMatrixColumnFromColumnName columnName model =
        model.Columns
        |> List.tryFind (_.Name >> (=) columnName)
        |> Option.map (
            _.MatrixIndex
            >> model.Matrix.Column
            >> Seq.toArray
        )
        |> Option.defaultValue Array.empty


    static member getColumnsToPlot model =
        Model.getMatrixColumnFromColumnName (model.ColumnsToPlot |> fst) model,
        Model.getMatrixColumnFromColumnName (model.ColumnsToPlot |> snd) model