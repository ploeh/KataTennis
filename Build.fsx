#r @"packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

Target "ResetAll" (fun _ ->
    directExec (fun info ->
        info.FileName <- "git"
        info.Arguments <- "clean -xdf")
    ()|> ignore)

Target "Clean" (fun _ ->
    CleanDirs ["KataTennis/bin"; "KataTennis/obj"]
)

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