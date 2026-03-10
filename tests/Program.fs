module WomanizerAcception.Scenario.Tests.Program
open Expecto
open Shilazeron.TestEngine
open Shilazeron.TestEngine.Helpers

open WomanizerAcception.Scenario
open WomanizerAcception.Scenario.SharedIds
open WomanizerAcception.Scenario.SharedIds.Objects
open WomanizerAcception.Scenario.SharedIds.Locations
open WomanizerAcception.Scenario.Tests.Utils

module Objects =
    open WomanizerAcception.Scenario.SharedIds.Objects

    let ты =
        object Ты.id
    let блондинка =
        object Блондинка.id

module LocationActions =
    open WomanizerAcception.Scenario.SharedIds.Objects

    let нажатьТы =
        selectObjectAction Ты.id

    let нажатьБлондинка =
        selectObjectAction Блондинка.id

open Objects
open LocationActions

[<Tests>]
let mainTest =
    testList "Engine.mainTest" [
        testCase "main" <| fun () ->
            let scenario = Scenario.scenario

            let reaction = start scenario
            let location, next = expectGoToLocation reaction

            location |> expectEqualLocationName Ресторан.id
            location |> expectEqualLocationDescription [
                [
                    ты "Ты" []
                    text "сидишь за "
                    link "столом" []
                    text "."
                ]
                [
                    text "Перед тобой сидит "
                    блондинка "блондиночка" [
                        action "Сказать комплимент"
                    ]
                    text "."
                ]
            ]
            ()
    ]

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
