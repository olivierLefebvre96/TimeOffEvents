module TimeOff.TestsRunner

open Expecto
open System

[<EntryPoint>]
let main args =
  runTestsWithArgs { defaultConfig with ``parallel`` = false } args Tests.tests
