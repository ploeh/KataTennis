#r @"packages/FAKE.4.9.3/tools/FakeLib.dll"

open Fake
open Fake.Testing

Target "Clean" (fun _ ->
    directExec (fun info ->
        info.FileName <- "git"
        info.Arguments <- "clean -xdf")
    |> ignore)

Target "Build" (fun _ ->
    !! "KataTennis.sln"
    |> MSBuildDebug "" "Rebuild"
    |> ignore)

Target "Test" (fun _ ->
    !! "*/bin/Debug/*PropertyBased.dll"
    |> xUnit2 id)

"Clean"
==> "Build"
==> "Test"

RunTargetOrDefault "Test"