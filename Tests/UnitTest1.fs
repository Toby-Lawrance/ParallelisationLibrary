module Tests

open System
open System.Diagnostics
open NUnit.Framework
open ParallelisationLibrary

let numThreads = Environment.ProcessorCount

let timeIt name (thing: 'X -> 'Y) arg =
    let stopWatch = Stopwatch.StartNew()
    let value = thing (arg)
    value |> ignore
    stopWatch.Stop()
    printfn "%s took: %f ms" name stopWatch.Elapsed.TotalMilliseconds
    stopWatch.Elapsed.TotalMilliseconds

[<SetUp>]
let Setup () = ()

[<Test>]
let CalltMap () =
    let numsToTransform = [ 1 .. 1000 ]
    let floatList = [ 1.0 .. 1000.0 ]
    let floated = List.pMap float numsToTransform
    Assert.AreEqual(floated, floatList)

[<Test>]
let SerialVsParallelBenchMark () =
    let task x =
        List.reduce (*) (List.map (fun y -> (x |> float) ** (y |> float)) [ 1 .. 100000 ])

    let range = [ 1 .. 100 ]
    let timeSerial = timeIt "Serial" (List.map task) range
    let timeParallel = timeIt "Parallel" (List.pMap task) range
    Assert.LessOrEqual(timeParallel, timeSerial)

[<Test>]
let RangeVsMapBenchmark () =
    let task x =
        List.reduce (*) (List.map (fun y -> (x |> float) ** (y |> float)) [ 1 .. 100 ])

    let timeMap =
        timeIt "Map" (List.pMap task) [ 1 .. 1000000 ]

    let timeRange = timeIt "Range" (List.pInit 100000) task
    Assert.LessOrEqual(timeRange, timeMap)

[<Test>]
let ``Show Correct offsets by generating a sequential list`` () =
    let task x = x
    let list = List.pInit 100000 task
    printfn ""
    printfn ""
    let normalList = List.init 100000 task
    Assert.AreEqual(list, normalList)

[<Test>]
let rangeGenVsSplit () =
    let num = 100000

    let genRanges x n =
        let spares = n % x
        let each = n / x

        let rec genRanges' don spare acc =
            let added =
                (don + each + (if spare > 0 then 1 else 0))

            let range = [ (don + 1) .. added ]
            match don with
            | _ when (added) >= n -> List.append acc [ range ]
            | _ -> genRanges' added (spare - 1) (List.append acc [ range ])

        genRanges' 0 spares []

    let range = [ 1 .. num ]

    let timeSplit =
        timeIt "Split" (List.splitInto numThreads) range

    let timeRange =
        timeIt "Range" (genRanges numThreads) num

    Assert.LessOrEqual(timeRange, timeSplit)

[<Test>]
let genIsSplit () =
    let genRanges x n =
        let spares = n % x
        let each = n / x

        let rec genRanges' don spare acc =
            let added =
                (don + each + (if spare > 0 then 1 else 0))

            let range = [ (don + 1) .. added ]
            match don with
            | _ when (added) >= n -> List.append acc [ range ]
            | _ -> genRanges' added (spare - 1) (List.append acc [ range ])

        genRanges' 0 spares []

    let num = 1000
    Assert.AreEqual(List.splitInto numThreads [ 1 .. num ], genRanges numThreads num)
