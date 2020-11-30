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
let ``Basic List.pMap usage`` () =
    let numsToTransform = [ 1 .. 1000 ]
    let floatList = [ 1.0 .. 1000.0 ]
    let floated = List.pMap float numsToTransform
    Assert.AreEqual(floated, floatList)

[<Test>]
let ``Basic Seq.pMap usage`` () =
    let numsToTransform = [ 1 .. 1000 ]
    let floatList = [ 1.0 .. 1000.0 ]
    let floated = Seq.pMap float numsToTransform
    Assert.AreEqual(floated, floatList)

[<Test>]
let ``Basic Array.pMap usage`` () =
    let numsToTransform = [| 1 .. 1000 |]
    let floatList = [| 1.0 .. 1000.0 |]
    let floated = Array.pMap float numsToTransform
    Assert.AreEqual(floated, floatList)

[<Test>]
let ``List pMap Serial Vs Parallel BenchMark`` () =
    let task x =
        List.reduce (*) (List.map (fun y -> (x |> float) ** (y |> float)) [ 1 .. 100000 ])

    let range = [ 1 .. 100 ]
    let timeSerial = timeIt "Serial" (List.map task) range
    let timeParallel = timeIt "Parallel" (List.pMap task) range
    Assert.LessOrEqual(timeParallel, timeSerial)

[<Test>]
let ``Seq pMap Serial Vs Parallel BenchMark`` () =
    let task x =
        Seq.reduce (*) (Seq.map (fun y -> (x |> float) ** (y |> float)) [ 1 .. 100000 ])

    let range = [ 1 .. 100 ]
    let timeSerial = timeIt "Serial" (Seq.map task) range
    let timeParallel = timeIt "Parallel" (Seq.pMap task) range
    Assert.LessOrEqual(timeParallel, timeSerial)

[<Test>]
let ``Array pMap Serial Vs Parallel BenchMark`` () =
    let task x =
        Array.reduce (*) (Array.map (fun y -> (x |> float) ** (y |> float)) [| 1 .. 100000 |])

    let range = [| 1 .. 100 |]
    let timeSerial = timeIt "Serial" (Array.map task) range

    let timeParallel =
        timeIt "Parallel" (Array.pMap task) range

    Assert.LessOrEqual(timeParallel, timeSerial)

[<Test>]
let ``List pInit Vs pMap Benchmark`` () =
    let task x =
        List.reduce (*) (List.map (fun y -> (x |> float) ** (y |> float)) [ 1 .. 100 ])

    let timeMap =
        timeIt "Map" (List.pMap task) [ 1 .. 1000000 ]

    let timeRange = timeIt "Range" (List.pInit 100000) task
    Assert.LessOrEqual(timeRange, timeMap)

[<Test>]
let ``Seq pInit Vs pMap Benchmark`` () =
    let task x =
        Seq.reduce (*) (Seq.map (fun y -> (x |> float) ** (y |> float)) [ 1 .. 100 ])

    let timeMap =
        timeIt "Map" (Seq.pMap task) [ 1 .. 1000000 ]

    let timeRange = timeIt "Range" (Seq.pInit 100000) task
    Assert.LessOrEqual(timeRange, timeMap)


[<Test>]
let ``Array pInit Vs pMap Benchmark`` () =
    let task x =
        Array.reduce (*) (Array.map (fun y -> (x |> float) ** (y |> float)) [| 1 .. 100 |])

    let timeMap =
        timeIt "Map" (Array.pMap task) [| 1 .. 1000000 |]

    let timeRange = timeIt "Range" (Array.pInit 100000) task
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
let ``Show Correct offsets by generating a sequential seq`` () =
    let task x = x
    let list = Seq.pInit 100000 task
    printfn ""
    printfn ""
    let normalList = Seq.init 100000 task
    Assert.AreEqual(list, normalList)

[<Test>]
let ``Show Correct offsets by generating a sequential array`` () =
    let task x = x
    let list = Array.pInit 100000 task
    printfn ""
    printfn ""
    let normalList = Array.init 100000 task
    Assert.AreEqual(list, normalList)

[<Test>]
let ``Range generation code is equivalent to Split`` () =
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
