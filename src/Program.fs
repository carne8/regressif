namespace Regressif

open Elmish
open Avalonia
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Markup.Xaml

type MainWindow() as this =
    inherit HostWindow()

    do
        #if DEBUG
        this.AttachDevTools()
        #endif

        this.Width <- 400.
        this.Height <- 400.

        Program.mkProgram State.init State.update View.view
        // |> Program.withConsoleTrace
        |> Program.withHost this
        |> Program.run


type App() =
    inherit Application()

    override this.Initialize() =
        AvaloniaXamlLoader.Load(this) // Used to load DataGrid theme because it doesn't work with with code and NativeAOT

        // this.Styles.Add(FluentTheme())
        // this.Styles.Load "avares://Avalonia.Controls.DataGrid/Themes/Fluent.xaml"
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

        base.OnFrameworkInitializationCompleted()


module Program =
    [<CompiledName "BuildAvaloniaApp">]
    let buildAvaloniaApp () =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)

    [<EntryPoint; System.STAThread>]
    let main argv =
        buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
