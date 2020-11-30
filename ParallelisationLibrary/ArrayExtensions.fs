namespace ParallelisationLibrary

open System

module Array =
    let private numThreads = Environment.ProcessorCount

    /// Uses the existing Array Parallel extension
    let pMap (mapper: 'T -> 'U) (l: 'T array): 'U array = Array.Parallel.map mapper l

    /// Maps two Arrays to at most the number of logical Processors and then executes the function
    /// The mapping function 'T -> 'T2 -> 'U is applied to each element
    /// returns a 'U Array in the same order as input
    let pMap2 (mapper: 'T -> 'T2 -> 'U) (l: 'T array) (l2: 'T2 array): 'U array =
        let asyncMap f xl yl = async { return Array.map2 f xl yl }
        let prepared l = Array.splitInto numThreads l
        Array.map2 (asyncMap mapper) (prepared l) (prepared l2)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.concat


    /// Maps three Arrays to at most the number of logical Processors and then executes the function
    /// The mapping function 'T -> 'T2 -> 'T3 -> 'U is applied to each element
    /// returns a 'U Array in the same order as input
    let pMap3 (mapper: 'T -> 'T2 -> 'T3 -> 'U) (l: 'T array) (l2: 'T2 array) (l3: 'T3 array): 'U array =
        let asyncMap f xl yl zl = async { return Array.map3 f xl yl zl }
        let prepared l = Array.splitInto numThreads l
        Array.map3 (asyncMap mapper) (prepared l) (prepared l2) (prepared l3)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.concat


    /// Uses existing Parallel extension for initialisation
    let pInit (n: int) (mapper: int -> 'U): 'U array = Array.Parallel.init n mapper


    /// Reduces a Array to a value in a parallel fashion NOTE: non-associative functions will have strange results. Does seem to be slower than single threaded for trivial operations
    /// The reducing function SHOULD BE ASSOCIATIVE
    /// Returns a single value of type 'T
    let pReduce (reducer: 'T -> 'T -> 'T) (l: 'T array): 'T =
        let asyncReduce f xl = async { return Array.reduce f xl }
        l
        |> Array.splitInto numThreads
        |> Array.map (asyncReduce reducer)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.reduce reducer
