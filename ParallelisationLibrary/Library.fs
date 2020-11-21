namespace ParallelisationLibrary

    open System

    module Extensions =
        let numThreads = Environment.ProcessorCount
        type Microsoft.FSharp.Collections.List<'T> with
        
            /// Maps a list to at most the number of logical Processors and then executes the function
            /// The mapping function 'T -> 'U is applied to each element
            /// returns a 'U list in the same order as input
            static member pMap (mapper: 'T -> 'U) (l: 'T list) : 'U list =
                let asyncMap f xl = async{return List.map f xl}
                l
                |> List.splitInto numThreads
                |> List.map (asyncMap mapper)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.concat
                
            /// Maps two lists to at most the number of logical Processors and then executes the function
            /// The mapping function 'T -> 'T2 -> 'U is applied to each element
            /// returns a 'U list in the same order as input
            static member pMap2 (mapper: 'T -> 'T2 -> 'U) (l: 'T list) (l2: 'T2 list) : 'U list =
                let asyncMap f xl yl = async{return List.map2 f xl yl}
                let prepared l = List.splitInto numThreads l
                List.map2 (asyncMap mapper) (prepared l) (prepared l2)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.concat
                
                
            /// Maps three lists to at most the number of logical Processors and then executes the function
            /// The mapping function 'T -> 'T2 -> 'T3 -> 'U is applied to each element
            /// returns a 'U list in the same order as input
            static member pMap3 (mapper: 'T -> 'T2 -> 'T3 -> 'U) (l: 'T list) (l2: 'T2 list) (l3: 'T3 list) : 'U list =
                let asyncMap f xl yl zl = async{return List.map3 f xl yl zl}
                let prepared l = List.splitInto numThreads l
                List.map3 (asyncMap mapper) (prepared l) (prepared l2) (prepared l3)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.concat
                
                
            /// Creates a split list from 1..n and then applies mapper to each element in a parallel way
            /// The mapping function from int -> 'U is applied to each element
            /// returns a 'U list in the same order as input
            static member pMapRange (mapper: int -> 'U) (n: int) : 'U list =
                let asyncMap f xl = async{return List.map f xl}
                let genRanges x n =
                    let spares = n % x
                    let each = n / x
                    let rec genRanges' don spare acc =
                        let added = (don + each + (if spare > 0 then 1 else 0))
                        let range = [(don + 1)..added]
                        match don with
                        | _ when (added) >= n -> List.append acc [range]
                        | _ -> genRanges' added (spare-1) (List.append acc [range])
                    genRanges' 0 spares []
                let ranges = genRanges numThreads n
                ranges
                |> List.map (asyncMap mapper)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.concat
                
            /// Reduces a list to a value in a parallel fashion NOTE: non-associative functions will have strange results. Does seem to be slower than single threaded for trivial operations
            /// The reducing function SHOULD BE ASSOCIATIVE
            /// Returns a single value of type 'T
            static member pReduce (reducer: 'T -> 'T -> 'T) (l: 'T list): 'T =
                let asyncReduce f xl = async{return List.reduce f xl}
                l
                |> List.splitInto numThreads
                |> List.map (asyncReduce reducer)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.reduce reducer