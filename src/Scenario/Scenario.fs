module WomanizerAcception.Scenario.Scenario

open Shilazeron
open Shilazeron.Operators
open Shilazeron.Helpers

open WomanizerAcception.Scenario.SharedIds

let milf: Object = {
    Id = milfId
    Name = Expr.str "Знойная продавщица"
    InitStates = []
    Actions = []
}

let аптека: Location = {
    Id = аптекаId
    Name = "Аптека"
    InitObjects = [milfId]
    Description = [
        sentence [
            text "За "
            link "прилавком" [
                action "Подойти" [

                ]
            ]
            text " снует "
            object milfId "знойная продавщица" [
                action "Осмотреть" []
            ]
            text "."
        ]
        sentence [
            link "Выход" [
                action "Выйти" [
                    Statement.Goto снаружиАптекиId
                ]
            ]
            text " на улицу."
        ]
    ]
}

let снаружиАптеки: Location = {
    Id = снаружиАптекиId
    Name = "У входа в аптеку"
    InitObjects = []
    Description = [
        sentence [
            text "Из витрины виднеется "
            link "кое-что" [
                action "Осмотреть" [
                    Statement.message "Это кое-что просто сводит тебя с ума. Ты долго собирался с духом, чтобы придти сюда и купить ЭТО."
                ]
            ]
            text "."
        ]
        sentence [
            link "Стеклянная дверь" [
                action "Войти" [
                    Statement.Goto аптекаId
                ]
            ]
            text " приглашает тебя внутрь."
        ]
    ]
}

let scenario : GameScenario =
    {
        Objects = [
            milf
        ] |> List.map (fun x -> x.Id, x) |> Map
        Locations = [
            снаружиАптеки
            аптека
        ] |> List.map (fun x -> x.Id, x) |> Map
        StartLocationId = снаружиАптекиId
        InitObjectIds = []
    }
