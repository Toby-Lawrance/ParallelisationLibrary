module Tests

open System.Diagnostics
open System.Xml.Linq
open NUnit.Framework
open ParallelisationLibrary.Extensions

let timeIt name (thing: 'X -> 'Y) arg =
    let stopWatch = Stopwatch.StartNew()
    let value = thing(arg)
    value |> ignore
    stopWatch.Stop()
    printfn "%s took: %f ms" name stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Elapsed.TotalMilliseconds

[<SetUp>]
let Setup () =
    ()

[<Test>]
let CalltMap () =
    let numsToTransform = [1..1000]
    let floatList = [1.0 .. 1000.0]
    let floated = List.pMap float numsToTransform
    Assert.AreEqual(floated,floatList)
    
[<Test>]
let BenchMark () =
    let task x = List.reduce (/) (List.map (fun y -> (x|>float)**(y|>float)) [1..1000000])
    let range = [1..1000]
    let timeSerial = timeIt "Serial" (List.map task) range
    let timeParallel = timeIt "Parallel" (List.pMap task) range
    Assert.LessOrEqual(timeParallel,timeSerial)