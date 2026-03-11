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

    let тарелкаСПирожками =
        object ТарелкаСПирожками.id

module Characters =
    open WomanizerAcception.Scenario.SharedIds.Characters

    let ты =
        object Ты.id

    let блондинка =
        object Блондинка.id

    let брюнетка =
        object Брюнетка.id

    let бабуля =
        object Бабуля.id

module LocationActions =
    open WomanizerAcception.Scenario.SharedIds.Characters
    open WomanizerAcception.Scenario.SharedIds.Objects

    let нажатьТы =
        selectObjectAction Ты.id

    let нажатьБлондинка =
        selectObjectAction Блондинка.id

    let нажатьБрюнетка =
        selectObjectAction Брюнетка.id

    let нажатьБабуля =
        selectObjectAction Бабуля.id

    let нажатьТарелкаСПирожками =
        selectObjectAction ТарелкаСПирожками.id

open Characters
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

            let location, next =
                (location, next)
                |> нажатьБлондинка "Сказать комплимент"
                |> expectRefreshLocation

            location |> expectEqualLocationName Ресторан.id
            location |> expectEqualLocationDescription [
                [
                    text "Слева от стола появляется до боли "
                    брюнетка "знакомая брюнеточка" [
                        action "Удивиться"
                    ]
                    text " и кричит на тебя: «Ловелас!»"
                    text "."
                ]
                [
                    блондинка "Блондиночка" []
                    text " смотрит на тебя округленными глазами."
                ]
            ]

            let location, next =
                (location, next)
                |> нажатьБрюнетка "Удивиться"
                |> expectRefreshLocation

            location |> expectEqualLocationName Ресторан.id
            location |> expectEqualLocationDescription [
                [
                    брюнетка "Брюнеточка" []
                    text " стоит со скрещенными руками и злобно смотрит на тебя."
                ]
                [
                    блондинка "Блондиночка" [
                        action "Отрицать"
                    ]
                    text " уточняет у тебя: «Ты ловелас?»"
                ]
            ]

            let location, next =
                (location, next)
                |> нажатьБлондинка "Отрицать"
                |> expectRefreshLocation

            location |> expectEqualLocationName Ресторан.id
            location |> expectEqualLocationDescription [
                [
                    блондинка "Блондиночка" []
                    text " и "
                    брюнетка "брюнеточка" []
                    text " переглядываются."
                ]
                [
                    text "Справа стоит "
                    бабуля "бабуля" [
                        action "Огрызнуться"
                    ]
                    text " и спрашивает: «Внучок, ты ловелас, што ле?»."
                ]
            ]

            let location, next =
                (location, next)
                |> нажатьБабуля "Огрызнуться"
                |> expectRefreshLocation

            location |> expectEqualLocationName Ресторан.id
            location |> expectEqualLocationDescription [
                [
                    блондинка "Блондиночка" []
                    text " с "
                    брюнетка "брюнеточкой" []
                    text " стоят возле стола и метают в друг друга молнии."
                ]
                [
                    бабуля "Бабуля" [
                        action "Наплести про выбор спутницы жизни"
                    ]
                    text " сидит за столом напротив тебя и спрашивает, как ты дошел до жизни такой."
                ]
            ]

            let location, next =
                (location, next)
                |> нажатьБабуля "Наплести про выбор спутницы жизни"
                |> expectRefreshLocation

            location |> expectEqualLocationName Ресторан.id
            location |> expectEqualLocationDescription [
                [
                    блондинка "Блондиночка" []
                    text " с "
                    брюнетка "брюнеточкой" []
                    text " хватают друг друга за волосы."
                ]
                [
                    бабуля "Бабуля" []
                    text " сидит напротив тебя с протянутой "
                    тарелкаСПирожками "тарелкой с пирожками" [
                        action "Взять"
                    ]
                    text "."
                ]
            ]

            let location, next =
                let obj, next =
                    (location, next)
                    |> нажатьТарелкаСПирожками "Взять"
                    |> expectAddObjectToHero
                obj |> expectEqualAddedObjectToHero Пирожок.id
                next |> expectRefreshLocation

            let location, next =
                let msg, next =
                    (location, next)
                    |> нажатьТарелкаСПирожками "Взять"
                    |> expectShowMessage
                msg |> expectEqualMessage "У тебя уже есть один."
                next |> expectRefreshLocation

            let location, next =
                let obj, next =
                    next
                    |> selectInventoryObjectAction Пирожок.id "Съесть"
                    |> expectRemoveObjectFromHero
                expectEqualRemovedObjectFromHero obj Пирожок.id
                next |> expectGoToLocation

            location |> expectEqualLocationName Комната.id
            location |> expectEqualLocationDescription [
                [
                    ты "Ты" [
                        action "Крепко задуматься"
                    ]
                    text " лежишь на кровати."
                ]
                [
                    блондинка "Блондиночка" []
                    text " обнимает тебя справа."
                ]
                [
                    брюнетка "Брюнеточка" []
                    text " обнимает тебя слева."
                ]
                [
                    text "Рядом с кроватью стоит "
                    бабуля "бабуля в неприличном одежде" []
                    text " с "
                    тарелкаСПирожками "тарелкой пирожков" []
                    text " и спрашивает: «Еще пирожков, старый разбойник?»."
                ]
            ]

            let location, next =
                (location, next)
                |> нажатьТы "Крепко задуматься"
                |> expectGoToLocation

            location |> expectEqualLocationName Конец.id
            location |> expectEqualLocationDescription [
                [
                    text "«Да и пофиг», — думаешь ты, обнимаешь двух прелестниц и уплетаешь бабулин пирожок."
                ]
                [
                    text "Спасибо, что поиграли! Надеемся, вам понравилось."
                ]
            ]
            ()
    ]

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
