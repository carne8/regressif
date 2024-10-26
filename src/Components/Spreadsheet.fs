namespace Regressif.Components

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Interactivity
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types

type CellOnEditedArgs(routedEvent, source, columnIdx, rowIdx, newText) =
    inherit RoutedEventArgs(routedEvent, source)

    member _.ColumnIdx = columnIdx
    member _.RowIdx = rowIdx
    member _.NewText = newText

type Cell(
    columnIdx: int,
    rowIdx: int
) as this =
    inherit UserControl()

    let textBox = TextBox()

    let mutable propertyInitialized = false
    let mutable beforeEditText = ""
    let evt = new Event<CellOnEditedArgs>()

    let onEditFinished _ =
        match not propertyInitialized || beforeEditText = textBox.Text with
        | true -> ()
        | false ->
            // Value changed
            beforeEditText <- textBox.Text

            CellOnEditedArgs(
                Cell.OnEditedEvent,
                this,
                columnIdx,
                rowIdx,
                textBox.Text
            )
            |> evt.Trigger

    do
        textBox.LostFocus.Add(onEditFinished)
        textBox.KeyDown.Add(fun evt ->
            match evt.Key = Input.Key.Enter with
            | true -> onEditFinished()
            | _ -> ()
        )

        this.Content <- textBox

    static member ShowOutlineProperty: StyledProperty<bool> = AvaloniaProperty.Register<Cell, bool>("ShowOutline", true)
    static member TextProperty: StyledProperty<string> = AvaloniaProperty.Register<Cell, string>("Text", "")
    static member OnEditedEvent : RoutedEvent<CellOnEditedArgs> =
        RoutedEvent.Register<Cell, CellOnEditedArgs>("OnEdited", RoutingStrategies.Bubble)

    member this.ShowOutline
        with get() = this.GetValue(Cell.ShowOutlineProperty)
        and set(value) = this.SetValue(Cell.ShowOutlineProperty, value) |> ignore

    member this.Text
        with get() = this.GetValue(Cell.TextProperty)
        and set(value) = this.SetValue(Cell.TextProperty, value) |> ignore

    [<CLIEvent>]
    member _.OnEdited = evt.Publish

    override _.OnPropertyChanged (change: AvaloniaPropertyChangedEventArgs): unit =
        base.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs)

        match change.Property.Name with
        | "ShowOutline" ->
            textBox.BorderThickness <-
                match change.NewValue |> unbox<bool> with
                | true -> Thickness(1)
                | false -> Thickness(0)

            // TODO: Set focused border style https://github.com/AvaloniaUI/Avalonia/discussions/5739

        | "Text" ->
            match propertyInitialized with
            | true -> ()
            | false ->
                propertyInitialized <- true
                beforeEditText <- change.NewValue |> unbox<string>

            textBox.Text <- change.NewValue |> unbox<string>
        | _ -> ()


type Spreadsheet() as this =
    inherit UserControl()

    let evt = new Event<CellOnEditedArgs>()
    let array = Array2D.init 3 3 (fun i j -> i + j)

    let rowItemsControl columnIdx (itemSource: float array) =
        ItemsControl(
            ItemsSource = (itemSource |> Array.indexed),
            ItemTemplate = FuncDataTemplate<int * float>(fun (rowIdx, value) _nameScope ->
                let cell =
                    Cell(
                        columnIdx,
                        rowIdx,
                        Text = (value |> string)
                    )

                cell.OnEdited.Add evt.Trigger

                cell
            )
        )

    let columnsItemsControl =
        ItemsControl(
            ItemTemplate =
                FuncDataTemplate<int * float[]>(
                    fun (columnIdx, column) _nameScope -> rowItemsControl columnIdx column
                ),
            ItemsPanel = FuncTemplate<Panel>(
                fun () -> StackPanel(Orientation = Layout.Orientation.Horizontal)
            )
        )

    do
        columnsItemsControl.ItemsSource <- (array |> Array2D.columns |> Array.indexed)

        this.Content <-
            ScrollViewer(
                HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto,
                Content = columnsItemsControl
            )

    static member DataProperty = AvaloniaProperty.Register<Spreadsheet, float array2d>("Data", Array2D.zeroCreate 0 0)
    static member OnEditedEvent = RoutedEvent.Register<Spreadsheet, CellOnEditedArgs>("OnEdited", RoutingStrategies.Bubble)

    member this.Data
        with get() = this.GetValue(Spreadsheet.DataProperty)
        and set(value) = this.SetValue(Spreadsheet.DataProperty, value) |> ignore

    [<CLIEvent>]
    member _.OnEdited = evt.Publish

    override _.OnPropertyChanged (change: AvaloniaPropertyChangedEventArgs): unit =
        base.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs)

        match change.Property.Name with
        | "Data" ->
            columnsItemsControl.ItemsSource <-
                change.GetNewValue<float array2d>()
                |> Array2D.columns
                |> Array.indexed
        | _ -> ()

// Avalonia FuncUI
module Spreadsheet =
    let create attrs = ViewBuilder.Create<Spreadsheet>(attrs)
    let data<'t when 't :> Spreadsheet> (value: float array2d) : IAttr<'t> =
        AttrBuilder<'t>.CreateProperty<float array2d>(Spreadsheet.DataProperty, value, ValueNone)


// type Spreadsheet() as this =
//     inherit UserControl()

//     let evt = new Event<CellOnEditedArgs>()
//     let grid = DataGrid()

//     do
//         grid.ColumnWidth <- DataGridLength.SizeToCells
//         this.Content <- grid

//     member _.CellTemplate columnIdx =
//         FuncDataTemplate<Row>(fun row _nameScope ->
//             let cell = Cell(columnIdx, row.Idx)

//             cell.Bind(Cell.TextProperty, Binding($"Row[{columnIdx}]")) |> ignore
//             cell.OnEdited.Add evt.Trigger

//             cell
//         )

//     static member DataProperty = AvaloniaProperty.Register<Spreadsheet, Array>("Data", Array.empty)
//     static member ColumnsProperty = AvaloniaProperty.Register<Spreadsheet, Dictionary<ColumnId, Column>>("Columns", System.Collections.Generic.Dictionary())
//     static member OnEditedEvent =
//         RoutedEvent.Register<Spreadsheet, CellOnEditedArgs>("OnEdited", RoutingStrategies.Bubble)

//     member _.Data
//         with get() = this.GetValue(Spreadsheet.DataProperty)
//         and set(value) = this.SetValue(Spreadsheet.DataProperty, value) |> ignore
//     member _.Columns
//         with get() = this.GetValue(Spreadsheet.ColumnsProperty)
//         and set(value) = this.SetValue(Spreadsheet.ColumnsProperty, value) |> ignore
//     [<CLIEvent>]
//     member _.OnEdited = evt.Publish

//     override _.OnPropertyChanged (change: AvaloniaPropertyChangedEventArgs): unit =
//         base.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs)

//         match change.Property.Name with
//         | "Columns" ->
//             grid.Columns.Clear()

//             change.NewValue
//             |> unbox<Dictionary<ColumnId, Column>>
//             |> Seq.iteri (fun columnIdx kv ->
//                 let column = kv.Value

//                 let el = DataGridTemplateColumn(
//                     Header = column.Name,
//                     CellTemplate = this.CellTemplate columnIdx,
//                     CellEditingTemplate = this.CellTemplate columnIdx
//                 )

//                 grid.Columns.Add(el)
//             )

//         | "Data" -> grid.ItemsSource <- change.NewValue |> unbox<Array>
//         | _ -> ()


//     // Avalonia FuncUI
//     static member create attrs = ViewBuilder.Create<Spreadsheet>(attrs)
//     static member data<'t when 't :> Spreadsheet> (value: float array array) : IAttr<'t> =
//         let newValue =
//             value
//             |> Array.mapi (fun idx row ->
//                 { Idx = idx
//                   Row = row |> Array.map string }
//             )

//         AttrBuilder<'t>.CreateProperty<Array>(Spreadsheet.DataProperty, newValue, ValueNone)

//     static member columns<'t when 't :> Spreadsheet> (value: Dictionary<ColumnId, Column>) : IAttr<'t> =
//         AttrBuilder<'t>.CreateProperty<Dictionary<ColumnId, Column>>(Spreadsheet.ColumnsProperty, value, ValueNone)

//     // Doesn't fire when the cell is edited
//     // static member onEdited<'t when 't :> Spreadsheet>(func: CellOnEditedArgs -> unit, ?subPatchOptions) =
//     //     AttrBuilder<'t>.CreateSubscription<CellOnEditedArgs>(Spreadsheet.OnEditedEvent, func, ?subPatchOptions = subPatchOptions)

