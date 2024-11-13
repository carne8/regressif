namespace Regressif.Components

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Interactivity
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.Styling
open System.Collections
open System

type CellOnEditedArgs(routedEvent, source, newText) =
    inherit RoutedEventArgs(routedEvent, source)
    member _.NewText = newText

type Cell(onEdited: string -> unit) as this =
    inherit UserControl()

    static let TextProperty = AvaloniaProperty.Register<Cell, string>("Text", "")
    static let ShowOutlineProperty: StyledProperty<bool> = AvaloniaProperty.Register<Cell, bool>("ShowOutline", true)

    let textBox = TextBox()

    let mutable beforeEditText = None

    let onEditFinished _ =
        match beforeEditText = Some textBox.Text with
        | true -> ()
        | false -> // Value changed
            beforeEditText <- Some textBox.Text
            onEdited textBox.Text

    // Used to hie textBox outline
    let cellStyle = Style(fun x ->
        x   // Selector="TextBox:focus /template/ Border#PART_BorderElement"
            .OfType<TextBox>()
            .Class(":focus")
            .Template()
            .OfType<Border>()
            .Name("PART_BorderElement")
        )

    do
        this.Styles.Add cellStyle

        // Bind TextProperty
        let textObservable =
            TextProperty
            |> this.GetObservable
            |> Observable.map (fun newText ->
                if newText <> "" && beforeEditText.IsNone then
                    beforeEditText <- Some newText
                newText
            )

        textBox.Bind(
            TextBox.TextProperty,
            textObservable
        )
        |> ignore

        // Connect events
        textBox.LostFocus.Add(onEditFinished)
        textBox.KeyDown.Add(fun evt ->
            match evt.Key = Input.Key.Enter with
            | true -> onEditFinished()
            | _ -> ()
        )

        this.Content <- textBox

    member this.Text
        with get() = this.GetValue(TextProperty)
        and set(value) = this.SetValue(TextProperty, value) |> ignore

    member this.ShowOutline
        with get() = this.GetValue(ShowOutlineProperty)
        and set(value) = this.SetValue(ShowOutlineProperty, value) |> ignore

    override _.OnPropertyChanged (change: AvaloniaPropertyChangedEventArgs): unit =
        base.OnPropertyChanged(change: AvaloniaPropertyChangedEventArgs)

        match change.Property.Name with
        | "ShowOutline" ->
            let thickness =
                match change.NewValue |> unbox<bool> with
                | true -> 1
                | false -> 0

            textBox.BorderThickness <- Thickness(thickness)

            cellStyle.Setters.Clear()
            cellStyle.Setters.Add(
                Setter(TextBox.BorderThicknessProperty, Thickness(thickness))
            )
        | _ -> ()


type SpreadsheetColumn =
    { Id: Regressif.ColumnId
      Index: int
      Name: string
      Values: float array }

type SpreadsheetOnValueEditedArgs(routedEvent, source, columnIdx, rowIdx, newText) =
    inherit RoutedEventArgs(routedEvent, source)

    member _.ColumnIdx = columnIdx
    member _.RowIdx = rowIdx
    member _.NewText = newText

type SpreadsheetOnColumnNameEditedArgs(routedEvent, source, columnId, newName) =
    inherit RoutedEventArgs(routedEvent, source)

    member _.ColumnId = columnId
    member _.NewName = newName

type Spreadsheet() as this =
    inherit UserControl()

    static let ItemsProperty = AvaloniaProperty.Register<Spreadsheet, SpreadsheetColumn array>("Items")
    static let OnValueEditedEvent =
        RoutedEvent.Register<Spreadsheet, SpreadsheetOnValueEditedArgs>(
            "OnValueEdited",
            RoutingStrategies.Bubble
        )
    static let OnColumnNameEditedEvent =
        RoutedEvent.Register<Spreadsheet, SpreadsheetOnColumnNameEditedArgs>(
            "OnColumnNameEdited",
            RoutingStrategies.Bubble
        )

    let valueEditedEvt = new Event<SpreadsheetOnValueEditedArgs>()
    let triggerValueEdited columnIdx rowIdx newText =
        SpreadsheetOnValueEditedArgs(
            OnValueEditedEvent,
            this,
            columnIdx,
            rowIdx,
            newText
        )
        |> valueEditedEvt.Trigger

    let columnNameEditedEvt = new Event<SpreadsheetOnColumnNameEditedArgs>()
    let triggerColumnNameEdited columnId newName =
        SpreadsheetOnColumnNameEditedArgs(
            OnColumnNameEditedEvent,
            this,
            columnId,
            newName
        )
        |> columnNameEditedEvt.Trigger

    let rowsItemsControl column =
        let border = Border()
        let stackPanel = StackPanel()

        let borderBrush =
            this.GetResourceObservable("SystemControlForegroundBaseMediumBrush")
            |> Observable.map (fun brush -> brush :?> Media.IBrush)

        border.Child <- stackPanel
        border.Bind(Border.BorderBrushProperty, borderBrush) |> ignore
        border.BorderThickness <-
            match column.Index with
            | 0 -> Thickness(1, 1, 1, 0)
            | _ -> Thickness(0, 1, 1, 0)

        border.VerticalAlignment <- Layout.VerticalAlignment.Top
        stackPanel.VerticalAlignment <- Layout.VerticalAlignment.Top

        stackPanel.Children.Add(
            Cell(
                (fun newText -> triggerColumnNameEdited column.Id newText),
                Text = column.Name,
                ShowOutline = false
            )
        )

        stackPanel.Children.Add(
            ItemsControl(
                ItemsSource = (column.Values |> Array.indexed),
                ItemTemplate = FuncDataTemplate<int * float>(fun (rowIdx, value) _nameScope ->
                    Cell(
                        (fun newText -> triggerValueEdited column.Index rowIdx newText),
                        Text = (value |> string),
                        ShowOutline = false
                    )
                    |> fun cell ->
                        let border =
                            Border(
                                Child = cell,
                                BorderThickness = (
                                    match rowIdx with
                                    | 0 -> Thickness(0, 1, 0, 1)
                                    | _ -> Thickness(0, 0, 0, 1)
                                )
                            )

                        border.Bind(Border.BorderBrushProperty, borderBrush) |> ignore
                        border
                )
            )
        )

        border

    let columnsItemsControl =
        ItemsControl(
            ItemTemplate = FuncDataTemplate<SpreadsheetColumn>(
                fun column _nameScope ->
                    rowsItemsControl column
            ),
            ItemsPanel = FuncTemplate<Panel>(fun () ->
                StackPanel(
                    Orientation = Layout.Orientation.Horizontal,
                    Margin = Thickness 1
                )
            )
        )

    do
        columnsItemsControl.Bind(
            ItemsControl.ItemsSourceProperty,
            ItemsProperty |> this.GetObservable :?> IObservable<IEnumerable>
        )
        |> ignore

        this.DataContext <- this
        this.Content <- columnsItemsControl

    member this.Items
        with get() = this.GetValue(ItemsProperty)
        and set(value) = this.SetValue(ItemsProperty, value) |> ignore

    member _.OnValueEdited = valueEditedEvt.Publish
    member _.OnColumnNameEdited = columnNameEditedEvt.Publish


    // Avalonia FuncUI
    static member create attrs = ViewBuilder.Create<Spreadsheet>(attrs)

    static member items<'t when 't :> Spreadsheet> value =
        AttrBuilder<'t>.CreateProperty(ItemsProperty, value, ValueNone)

    static member items<'t when 't :> Spreadsheet> (columns: Regressif.Column array, array: float array2d) =
        array
        |> Array2D.columns
        |> Array.mapi (fun columnIdx values ->
            let column =
                columns
                |> Array.tryItem columnIdx
                |> Option.defaultWith (fun _ -> failwith "Column not found")

            { Id = column.Id
              Index = columnIdx
              Name = column.Name
              Values = values }
        )
        |> Spreadsheet.items