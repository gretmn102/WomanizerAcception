module WomanizerAcception.Scenario.Tests.Program
open Expecto
open Shilazeron.TestEngine
open Shilazeron.TestEngine.Helpers

open WomanizerAcception.Scenario
open WomanizerAcception.Scenario.SharedIds
open WomanizerAcception.Scenario.Tests.Utils

[<Tests>]
let mainTest =
    testList "Engine.mainTest" [
        testCase "main" <| fun () ->
            let scenario = Scenario.scenario

            let reaction = start scenario
            let location, next = expectGoToLocation reaction

            location |> expectEqualLocationName снаружиАптекиId
            location |> expectEqualLocationDescription [
                [
                    text "Из витрины виднеется "
                    link "кое-что" [
                        action "Осмотреть"
                    ]
                    text "."
                ]
                [
                    link "Стеклянная дверь" [
                        action "Войти"
                    ]
                    text " приглашает тебя внутрь."
                ]
            ]
            let next = (location, next) |> selectLinkAction "кое-что" "Осмотреть"
            let msg, next = next |> expectShowMessage
            Expect.equal "Это кое-что просто сводит тебя с ума. Ты долго собирался с духом, чтобы придти сюда и купить ЭТО." msg ""
            let location, next = expectRefreshLocation next
            let next = (location, next) |> selectLinkAction "Стеклянная дверь" "Войти"
            let location, next = expectGoToLocation next

            location |> expectEqualLocationName аптекаId
            location |> expectEqualLocationDescription [
                [
                    text "За "
                    link "прилавком" [
                        action "Подойти"
                    ]
                    text " снует "
                    object milfId "знойная продавщица" [
                        action "Осмотреть"
                    ]
                    text "."
                ]
                [
                    link "Выход" [
                        action "Выйти"
                    ]
                    text " на улицу."
                ]
            ]
            ()
    ]

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
