namespace ParallelisationLibrary

    open System

    module Extensions =
        type Microsoft.FSharp.Collections.List<'T> with
            static member pMap (mapper: 'T -> 'U) (l: 'T list) : 'U list =
                let asyncMap f xl = async{return List.map f xl}
                let numThreads = Environment.ProcessorCount
                l
                |> List.splitInto numThreads
                |> List.map (asyncMap mapper)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.concat
                