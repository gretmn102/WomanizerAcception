#!dotnet fsi
#r "nuget: Fake.Core.Target, 6.0"
#r "nuget: Fake.DotNet.Cli, 6.0"
#r "nuget: Fake.IO.FileSystem, 6.0"
#r "nuget: Fake.Core.ReleaseNotes, 6.0"

module Utils =
    open Fake.Core
    open Fake.DotNet
    open Fake.IO
    open Fake.IO.FileSystemOperators
    open Fake.IO.Globbing.Operators

    let npm args workingDir =
        let npmPath =
            match ProcessUtils.tryFindFileOnPath "npm" with
            | Some path -> path
            | None ->
                "npm was not found in path. Please install it and make sure it's available from your path. " +
                "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
                |> failwith

        let arguments = args |> String.split ' ' |> Arguments.OfArgs

        Command.RawCommand (npmPath, arguments)
        |> CreateProcess.fromCommand
        |> CreateProcess.withWorkingDirectory workingDir
        |> CreateProcess.ensureExitCode
        |> Proc.run
        |> ignore

    let dotnet cmd workingDir =
        let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
        if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

    let removeBinAndObj path =
        try
            Shell.cleanDirs [
                path </> "bin"
                path </> "obj"
            ]
        with e ->
            printfn "%A" e.Message

    let initFakeRuntime () =
        System.Environment.GetCommandLineArgs()
        |> Array.skip 2 // skip fsi.exe; build.fsx
        |> Array.toList
        |> Context.FakeExecutionContext.Create false __SOURCE_FILE__
        |> Context.RuntimeContext.Fake
        |> Context.setExecutionContext

        Target.initEnvironment ()

    let findPackPath dir =
        let packPathPattern =
            dir </> "*.nupkg"

        !! packPathPattern
        |> Seq.truncate 2
        |> List.ofSeq
        |> function
            | [nupkgPath] -> nupkgPath
            | [] ->
                failwithf "'%s' not found" packPathPattern
            | nupkgPaths ->
                failwithf "More than one *.nupkg found: '%A'" nupkgPaths

module Fable =
    open Fake.Core
    open Fake.IO

    open Utils

    type BuildOption =
        {
            Output: string
            IsWatch: bool
            NoRestore: bool
        }
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    [<RequireQualifiedAccess>]
    module BuildOption =
        let empty: BuildOption =
            {
                Output = ""
                IsWatch = false
                NoRestore = false
            }

    let run fableArgs =
        dotnet (sprintf "fable %s" (String.concat " " fableArgs)) "."

    let build (fableOption: BuildOption) projectPath =
        [
            yield projectPath

            if fableOption.NoRestore then
                yield "--noRestore"

            yield
                match fableOption.Output with
                | null | "" -> ""
                | output -> sprintf "-o %s" output

            if fableOption.IsWatch then
                yield "--watch"
        ]
        |> run

    let clean outputPath =
        if Shell.testDir outputPath then
            [
                "clean"
                sprintf "-o %s" outputPath
                "--yes"
            ]
            |> run

module XmlText =
    let escape rawText =
        let doc = new System.Xml.XmlDocument()
        let node = doc.CreateElement("root")
        node.InnerText <- rawText
        node.InnerXml

Utils.initFakeRuntime ()

module GameProject =
    open Fake.Core
    open Fake.Core.TargetOperators
    open Fake.IO
    open Fake.IO.FileSystemOperators

    open Utils

    let projectDirectory = Path.getFullName "./src/Game"
    let fableDirectory = projectDirectory </> "bin" </> "Fable"
    let deployDirectory = Path.getFullName "./deploy"

    let prefix = "Game"

    let commonBuildArgs = "-c Release"

    let dotnetBuildTarget = prefix + "DotnetBuild"
    Target.create dotnetBuildTarget (fun _ ->
        projectDirectory
        |> dotnet (sprintf "build %s" commonBuildArgs)
    )

    let dotnetCleanTarget = prefix + "DotnetClean"
    Target.create dotnetCleanTarget (fun _ ->
        removeBinAndObj projectDirectory
    )

    let deployCleanTarget = prefix + "DeployClean"
    Target.create deployCleanTarget (fun _ ->
        Shell.cleanDir deployDirectory
    )

    let deployTarget = prefix + "Deploy"
    Target.create deployTarget (fun _ -> ())

    let fableCleanTarget = prefix + "FableClean"
    Target.create fableCleanTarget (fun _ ->
        Fable.clean fableDirectory
    )

    let fableBuildTarget = prefix + "FableBuild"
    Target.create fableBuildTarget (fun _ ->
        projectDirectory
        |> Fable.build
            { Fable.BuildOption.empty with
                Output = fableDirectory
            }
    )

    let fableWatchTarget = prefix + "FableWatch"
    Target.create fableWatchTarget (fun _ ->
        projectDirectory
        |> Fable.build
            { Fable.BuildOption.empty with
                Output = fableDirectory
                IsWatch = true
            }
    )

    let viteWatchTarget = prefix + "ViteWatch"
    Target.create viteWatchTarget (fun _ ->
        projectDirectory
        |> npm "run vite:watch"
    )

    let viteBuildTarget = prefix + "ViteBuild"
    Target.create viteBuildTarget (fun _ ->
        projectDirectory
        |> npm $"run vite:build -- --base ./ --outDir {deployDirectory}"
    )

    fableCleanTarget ?=> fableBuildTarget
    fableCleanTarget ==> viteBuildTarget
    fableBuildTarget ==> viteBuildTarget
    deployCleanTarget ?=> viteBuildTarget
    deployCleanTarget ==> deployTarget
    viteBuildTarget ==> deployTarget

module TestsProject =
    open Fake.Core
    open Fake.Core.TargetOperators
    open Fake.IO
    open Fake.IO.FileSystemOperators

    open Utils

    let projectDirectory = Path.getFullName "./tests"

    let prefix = "Tests"

    let dotnetCleanTarget = prefix + "DotnetClean"
    Target.create dotnetCleanTarget (fun _ ->
        removeBinAndObj projectDirectory
    )

    let dotnetRunTarget = prefix + "DotnetRun"
    Target.create dotnetRunTarget (fun _ ->
        projectDirectory
        |> dotnet "run"
    )

open Fake.Core
open Fake.Core.TargetOperators

let cleanTarget = "Clean"
Target.create cleanTarget (fun _ -> ())

GameProject.deployCleanTarget ==> cleanTarget
GameProject.dotnetCleanTarget ==> cleanTarget
GameProject.fableCleanTarget ==> cleanTarget
TestsProject.dotnetCleanTarget ==> cleanTarget

try
    Target.runOrDefaultWithArguments GameProject.deployTarget
with _ ->
    System.Environment.Exit(1)
