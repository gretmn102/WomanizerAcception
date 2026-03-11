module WomanizerAcception.Scenario.Scenario

open Shilazeron
open Shilazeron.Operators
open Shilazeron.Helpers

open WomanizerAcception.Scenario.SharedIds

module CharacterAliases =
    open WomanizerAcception.Scenario.SharedIds.Characters

    let objectIf id description =
        SentenceStatement.objectIf id (Expr.str description)

    let ты = object Ты.id
    let тыIf = objectIf Ты.id

    let блондинка = object Блондинка.id
    let блондинкаIf = objectIf Блондинка.id

    let брюнетка = object Брюнетка.id
    let брюнеткаIf = objectIf Брюнетка.id

    let бабуля = object Бабуля.id
    let бабуляIf = objectIf Бабуля.id

open CharacterAliases

module Objects =
    module ТарелкаСПирожками =
        open WomanizerAcception.Scenario.SharedIds.Objects.ТарелкаСПирожками

        let object: Object = {
            Id = id
            Name = Expr.str "Тарелка с пирожками"
            InitStates = []
            Actions = []
        }

module Characters =
    module Ты =
        open WomanizerAcception.Scenario.SharedIds.Characters.Ты

        let object: Object = {
            Id = id
            Name = Expr.str "Ты"
            InitStates = []
            Actions = []
        }

    module Блондинка =
        open WomanizerAcception.Scenario.SharedIds.Characters.Блондинка

        let object: Object = {
            Id = id
            Name = Expr.str "Блондинка"
            InitStates = []
            Actions = []
        }

    module Брюнетка =
        open WomanizerAcception.Scenario.SharedIds.Characters.Брюнетка

        let object: Object = {
            Id = id
            Name = Expr.str "Брюнетка"
            InitStates = []
            Actions = []
        }

    module Бабуля =
        open WomanizerAcception.Scenario.SharedIds.Characters.Бабуля

        let object: Object = {
            Id = id
            Name = Expr.str "Бабуля"
            InitStates = []
            Actions = []
        }

module Locations =
    open Characters
    open Objects

    module Ресторан =
        open WomanizerAcception.Scenario.SharedIds.Locations.Ресторан

        let location: Location = {
            Id = id
            Name = "Ресторан"
            InitObjects = [Ты.id; Блондинка.id]
            Description = [
                sentence [
                    тыIf "Ты" (
                        Expr.not <| Expr.thisLocationHasObject Брюнетка.id
                    ) []
                    text "сидишь за "
                    link "столом" []
                    text "."
                ]
                oneOf [
                    sentence [
                        text "Слева от стола появляется до боли "
                        брюнеткаIf "знакомая брюнеточка" (
                            Expr.thisObjectHasState Брюнетка.узнаетЧтоТыНаСвиданииСДругой
                        ) [
                            action "Удивиться" [
                                Statement.removeObjectState Брюнетка.id Брюнетка.узнаетЧтоТыНаСвиданииСДругой
                                Statement.addObjectState Брюнетка.id Брюнетка.срещиваетРуки

                                Statement.removeObjectState Блондинка.id Блондинка.узнаетОСуществованииБрюнетки
                                Statement.addObjectState Блондинка.id Блондинка.уточняетЛовеласЛиТы
                            ]
                        ]
                        text " и кричит на тебя: «Ловелас!»"
                        text "."
                    ]
                    sentence [
                        брюнеткаIf "Брюнеточка" (
                            Expr.thisObjectHasState Брюнетка.срещиваетРуки
                        ) []
                        text " стоит со скрещенными руками и злобно смотрит на тебя."
                    ]
                ]
                oneOf [
                    sentence [
                        text "Перед тобой сидит "
                        блондинкаIf "блондиночка" (
                            Expr.not <| Expr.thisLocationHasObject Брюнетка.id
                        ) [
                            action "Сказать комплимент" [
                                Statement.addObjectState Брюнетка.id Брюнетка.узнаетЧтоТыНаСвиданииСДругой
                                Statement.addObjectToThisLocation Брюнетка.id
                                Statement.addObjectState Блондинка.id Блондинка.узнаетОСуществованииБрюнетки
                            ]
                        ]
                        text "."
                    ]
                    sentence [
                        блондинкаIf "Блондиночка" (
                            Expr.thisObjectHasState Блондинка.узнаетОСуществованииБрюнетки
                        ) []
                        text " смотрит на тебя округленными глазами."
                    ]
                    sentence [
                        блондинкаIf "Блондиночка" (
                            Expr.thisObjectHasState Блондинка.уточняетЛовеласЛиТы
                        ) [
                            action "Отрицать" [
                                Statement.removeObjectState Блондинка.id Блондинка.уточняетЛовеласЛиТы
                                Statement.addObjectState Блондинка.id Блондинка.думаетЧтоБрюнеткаЛжет

                                Statement.removeObjectState Брюнетка.id Брюнетка.срещиваетРуки
                                Statement.addObjectState Брюнетка.id Брюнетка.думаетЧтоБлондинкаЕйНеВерит

                                Statement.addObjectState Бабуля.id Бабуля.суетНосНеВСвоиДела
                                Statement.addObjectToThisLocation Бабуля.id
                            ]
                        ]
                        text " уточняет у тебя: «Ты ловелас?»"
                    ]
                ]
                oneOf [
                    sentence [
                        блондинкаIf "Блондиночка" (
                            Expr.thisObjectHasState Блондинка.думаетЧтоБрюнеткаЛжет
                        ) []
                        text " и "
                        брюнеткаIf "брюнеточка" (
                            Expr.thisObjectHasState Брюнетка.думаетЧтоБлондинкаЕйНеВерит
                        ) []
                        text " переглядываются."
                    ]
                ]
                oneOf [
                    sentence [
                        text "Справа стоит "
                        бабуляIf "бабуля" (
                            Expr.thisObjectHasState Бабуля.суетНосНеВСвоиДела
                        ) [
                            action "Огрызнуться" [
                                Statement.removeObjectState Бабуля.id Бабуля.суетНосНеВСвоиДела
                                Statement.removeObjectState Блондинка.id Блондинка.думаетЧтоБрюнеткаЛжет
                                Statement.removeObjectState Брюнетка.id Брюнетка.думаетЧтоБлондинкаЕйНеВерит
                            ]
                        ]
                        text " и спрашивает: «Внучок, ты ловелас, што ле?»."
                    ]
                ]
            ]
        }

    module Комната =
        open WomanizerAcception.Scenario.SharedIds.Locations.Комната

        let location: Location = {
            Id = id
            Name = "Комната"
            InitObjects = []
            Description = []
        }

    module Конец =
        open WomanizerAcception.Scenario.SharedIds.Locations.Конец

        let location: Location = {
            Id = id
            Name = "Конец"
            InitObjects = []
            Description = []
        }

let scenario : GameScenario =
    {
        Objects = [
            Objects.ТарелкаСПирожками.object
            Characters.Ты.object
            Characters.Блондинка.object
            Characters.Брюнетка.object
            Characters.Бабуля.object
        ] |> List.map (fun x -> x.Id, x) |> Map
        Locations = [
            Locations.Ресторан.location
            Locations.Комната.location
            Locations.Конец.location
        ] |> List.map (fun x -> x.Id, x) |> Map
        StartLocationId = Locations.Ресторан.id
        InitObjectIds = []
    }
