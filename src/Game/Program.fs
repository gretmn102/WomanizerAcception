module WomanizerAcception.Game.TestGame
open Elmish
open Elmish.React
#if DEBUG
open Elmish.HMR
#endif
open Thekla.Player.Index
open WomanizerAcception.Scenario.Scenario

Fable.Core.JsInterop.import "" "./index.css"

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "app"
|> Program.runWith scenario
