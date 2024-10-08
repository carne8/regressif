namespace Regressif

open MathNet.Numerics.LinearAlgebra

[<RequireQualifiedAccess>]
type ColumnType =
    | Values
    | Formula of string

type Column =
    { Name: string
      /// The index of the column in the matrix
      MatrixIndex: int
      Type: ColumnType }

[<RequireQualifiedAccess>]
type RawMatrixManipMsg =
    | AddRow
    | RemoveRow of rowIdx: int
    | AddColumn of Column
    | RemoveColumn of columnIdx: int

[<RequireQualifiedAccess>]
type Msg =
    | CellEdited of columnIdx: int * rowIdx: int * string
    | RawMatrixManip of RawMatrixManipMsg

type Model =
    { Columns: Column list
      Matrix: Matrix<float>
      MatrixLastGenerationId: uint }