namespace Regressif

open MathNet.Numerics.LinearAlgebra

[<RequireQualifiedAccess>]
type ColumnType =
    | Values
    | Formula of string

type Column =
    { Name: string
      Type: ColumnType }

type Msg =
    | AddColumn of Column
    | RemoveColumn of columnName: string
    | AddRow
    | RemoveRow of rowIdx: int
    | EditValue of columnName: string * rowIdx: int * float

type Model =
    { Columns: Column list
      Values: Matrix<float> }