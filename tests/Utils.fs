module WomanizerAcception.Scenario.Tests.Utils
open Expecto
open Shilazeron.TestEngine
open Shilazeron.TestEngine.Helpers

open WomanizerAcception.Scenario.Scenario

let expectEqualLocationName locationId (engineLocation: Shilazeron.Engine.SetLocationArguments) =
    match Map.tryFind locationId scenario.Locations with
    | Some location ->
        Expect.equal location.Name engineLocation.Name ""
    | None ->
        failtestf "Not found location by %A id" locationId

// refactor: use https://github.com/lapkiteam/Shilazeron.TestEngine/issues/16
let expectEqualLocationDescription expectedDescription (location: Shilazeron.Engine.SetLocationArguments) =
    Expect.equal (ofEngine location.Description) expectedDescription ""

// refactor: use https://github.com/lapkiteam/Shilazeron.TestEngine/issues/17
let expectEqualAddedObjectToHero expectedObjectId (addedObject: Shilazeron.Engine.Statement.AddObjectToHeroArgs) =
    Expect.equal addedObject.Id expectedObjectId ""

// refactor: use https://github.com/lapkiteam/Shilazeron.TestEngine/issues/18
let expectEqualRemovedObjectFromHero (removedObjectId: Shilazeron.ObjectId) expectedObjectId =
    Expect.equal removedObjectId expectedObjectId ""

// refactor: use https://github.com/lapkiteam/Shilazeron.TestEngine/issues/19
let expectEqualMessage (act: string) exp =
    Expect.equal act exp ""

let selectAndMessage select action message next =
    let next = next |> select action
    let msg, next = next |> expectShowMessage
    msg |> expectEqualMessage message
    next |> expectRefreshLocation
