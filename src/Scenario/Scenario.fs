module WomanizerAcception.Scenario.Scenario

open Shilazeron
open Shilazeron.Operators
open Shilazeron.Helpers

open WomanizerAcception.Scenario.SharedIds

module Statement =
    let moveObjectToLocation objectId locationId =
        [
            Statement.removeObjectFromThisLocation objectId
            Statement.addObjectToLocation objectId locationId
        ]

module ObjectAliases =
    open WomanizerAcception.Scenario.SharedIds.Objects

    let тарелкаСПирожками = object ТарелкаСПирожками.id

    let пирожок = object Пирожок.id

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
open ObjectAliases

module Objects =
    module ТарелкаСПирожками =
        open WomanizerAcception.Scenario.SharedIds.Objects.ТарелкаСПирожками

        let object: Object = {
            Id = id
            Name = Expr.str "Тарелка с пирожками"
            InitStates = []
            Actions = []
        }

    module Пирожок =
        open WomanizerAcception.Scenario.SharedIds.Objects
        open WomanizerAcception.Scenario.SharedIds.Objects.Пирожок
        open WomanizerAcception.Scenario.SharedIds.Characters

        let object: Object = {
            Id = id
            Name = Expr.str "Пирожок"
            InitStates = []
            Actions = [
                action "Съесть" [
                    Statement.RemoveObjectFromHero id

                    Statement.removeObjectState Блондинка.id Блондинка.держитВолосыБрюнетки
                    Statement.removeObjectState Брюнетка.id Брюнетка.держитВолосыБлондинки
                    Statement.removeObjectState Бабуля.id Бабуля.предлагаетСпасительныйПирожок

                    yield! Statement.moveObjectToLocation Блондинка.id Locations.Комната.id
                    yield! Statement.moveObjectToLocation Брюнетка.id Locations.Комната.id
                    yield! Statement.moveObjectToLocation Бабуля.id Locations.Комната.id
                    yield! Statement.moveObjectToLocation ТарелкаСПирожками.id Locations.Комната.id
                    yield! Statement.moveObjectToLocation Ты.id Locations.Комната.id
                    Statement.Goto Locations.Комната.id
                ]
            ]
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
                    sentence [
                        блондинкаIf "Блондиночка" (
                            Expr.thisObjectHasState Блондинка.метаетМолнииВБрюнетку
                        ) []
                        text " с "
                        брюнеткаIf "брюнеточкой" (
                            Expr.thisObjectHasState Брюнетка.метаетМолнииВБлондинку
                        ) []
                        text " стоят возле стола и метают в друг друга молнии."
                    ]
                    sentence [
                        блондинкаIf "Блондиночка" (
                            Expr.thisObjectHasState Блондинка.держитВолосыБрюнетки
                        ) []
                        text " с "
                        брюнеткаIf "брюнеточкой" (
                            Expr.thisObjectHasState Брюнетка.держитВолосыБлондинки
                        ) []
                        text " хватают друг друга за волосы."
                    ]
                ]
                oneOf [
                    sentence [
                        text "Справа стоит "
                        бабуляIf "бабуля" (
                            Expr.thisObjectHasState Бабуля.суетНосНеВСвоиДела
                        ) [
                            action "Огрызнуться" [
                                Statement.removeObjectState Блондинка.id Блондинка.думаетЧтоБрюнеткаЛжет
                                Statement.addObjectState Блондинка.id Блондинка.метаетМолнииВБрюнетку

                                Statement.removeObjectState Брюнетка.id Брюнетка.думаетЧтоБлондинкаЕйНеВерит
                                Statement.addObjectState Брюнетка.id Брюнетка.метаетМолнииВБлондинку

                                Statement.removeObjectState Бабуля.id Бабуля.суетНосНеВСвоиДела
                                Statement.addObjectState Бабуля.id Бабуля.спрашиваетКакТыДошелДоТакого
                            ]
                        ]
                        text " и спрашивает: «Внучок, ты ловелас, што ле?»."
                    ]
                    sentence [
                        бабуляIf "Бабуля" (
                            Expr.thisObjectHasState Бабуля.спрашиваетКакТыДошелДоТакого
                        ) [
                            action "Наплести про выбор спутницы жизни" [
                                Statement.removeObjectState Блондинка.id Блондинка.метаетМолнииВБрюнетку
                                Statement.addObjectState Блондинка.id Блондинка.держитВолосыБрюнетки

                                Statement.removeObjectState Брюнетка.id Брюнетка.метаетМолнииВБлондинку
                                Statement.addObjectState Брюнетка.id Брюнетка.держитВолосыБлондинки

                                Statement.removeObjectState Бабуля.id Бабуля.спрашиваетКакТыДошелДоТакого
                                Statement.addObjectState Бабуля.id Бабуля.предлагаетСпасительныйПирожок

                                Statement.addObjectToThisLocation ТарелкаСПирожками.id
                            ]
                        ]
                        text " сидит за столом напротив тебя и спрашивает, как ты дошел до жизни такой."
                    ]
                    sentence [
                        бабуляIf "Бабуля" (
                            Expr.thisObjectHasState Бабуля.предлагаетСпасительныйПирожок
                        ) []
                        text " сидит напротив тебя с протянутой "
                        тарелкаСПирожками "тарелкой с пирожками" [
                            action "Взять" [
                                Statement.if' (
                                    Expr.not <| Expr.heroHas Пирожок.id
                                ) [
                                    Statement.take Пирожок.id
                                ] [
                                    Statement.message "У тебя уже есть один."
                                ]
                            ]
                        ]
                        text "."
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
            Description = [
                sentence [
                    ты "Ты" [
                        action "Крепко задуматься" []
                    ]
                    text " лежишь на кровати."
                ]
                sentence [
                    блондинка "Блондиночка" []
                    text " обнимает тебя справа."
                ]
                sentence [
                    брюнетка "Брюнеточка" []
                    text " обнимает тебя слева."
                ]
                sentence [
                    text "Рядом с кроватью стоит "
                    бабуля "бабуля в неприличном одежде" []
                    text " с "
                    тарелкаСПирожками "тарелкой пирожков" [
                        action "Взять" [
                            Statement.Goto Locations.Конец.id
                        ]
                    ]
                    text " и спрашивает: «Еще пирожков, старый разбойник?»."
                ]
            ]
        }

    module Конец =
        open WomanizerAcception.Scenario.SharedIds.Locations.Конец

        let location: Location = {
            Id = id
            Name = "Конец"
            InitObjects = []
            Description = [
                sentence [
                    text "Спасибо за игру! Надеемся, вам понравилось."
                ]
            ]
        }

let scenario : GameScenario =
    {
        Objects = [
            Objects.ТарелкаСПирожками.object
            Objects.Пирожок.object
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
