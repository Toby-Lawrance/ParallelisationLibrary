namespace ParallelisationLibrary

    open System
    open System.Diagnostics.CodeAnalysis

    module Seq =
            let private numThreads = Environment.ProcessorCount
            
            /// Maps a Seq to at most the number of logical Processors and then executes the function
            /// The mapping function 'T -> 'U is applied to each element
            /// returns a 'U Seq in the same order as input
            let pMap (mapper: 'T -> 'U) (l: 'T seq) : 'U seq =
                let asyncMap f xl = async{return Seq.map f xl}
                l
                |> Seq.splitInto numThreads
                |> Seq.map (asyncMap mapper)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toSeq
                |> Seq.concat
                
            /// Maps two Seqs to at most the number of logical Processors and then executes the function
            /// The mapping function 'T -> 'T2 -> 'U is applied to each element
            /// returns a 'U Seq in the same order as input
            let pMap2 (mapper: 'T -> 'T2 -> 'U) (l: 'T seq) (l2: 'T2 seq) : 'U seq =
                let asyncMap f xl yl = async{return Seq.map2 f xl yl}
                let prepared l = Seq.splitInto numThreads l
                Seq.map2 (asyncMap mapper) (prepared l) (prepared l2)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toSeq
                |> Seq.concat
                
                
            /// Maps three Seqs to at most the number of logical Processors and then executes the function
            /// The mapping function 'T -> 'T2 -> 'T3 -> 'U is applied to each element
            /// returns a 'U Seq in the same order as input
            let pMap3 (mapper: 'T -> 'T2 -> 'T3 -> 'U) (l: 'T seq) (l2: 'T2 seq) (l3: 'T3 seq) : 'U seq =
                let asyncMap f xl yl zl = async{return Seq.map3 f xl yl zl}
                let prepared l = Seq.splitInto numThreads l
                Seq.map3 (asyncMap mapper) (prepared l) (prepared l2) (prepared l3)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toSeq
                |> Seq.concat
                
                
            /// Same as Seq.init but in a parallel fashion
            /// Appears to run significantly faster under most loads
            let pInit (n: int) (mapper: int -> 'U) : 'U seq =
                let asyncMap f count = async{return Seq.init count f}
                let applyOffset off (f : int -> 'U) n = f (off + n)  
                let genRanges x n =
                    let spares = n % x
                    let each = n / x
                    let rec genRanges' don spare acc =
                        let added = (don + each + (if spare > 0 then 1 else 0))
                        match don with
                        | _ when (added) >= n -> Seq.append acc [(don,added-don)]
                        | _ -> genRanges' added (spare-1) (Seq.append acc [(don,added-don)])
                    genRanges' 0 spares []
                let ranges = genRanges numThreads n
                ranges
                |> Seq.map (fun (off,count) -> asyncMap (applyOffset off mapper) count)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toSeq
                |> Seq.concat
                
                
            /// Reduces a Seq to a value in a parallel fashion NOTE: non-associative functions will have strange results. Does seem to be slower than single threaded for trivial operations
            /// The reducing function SHOULD BE ASSOCIATIVE
            /// Returns a single value of type 'T
            let pReduce (reducer: 'T -> 'T -> 'T) (l: 'T seq): 'T =
                let asyncReduce f xl = async{return Seq.reduce f xl}
                l
                |> Seq.splitInto numThreads
                |> Seq.map (asyncReduce reducer)
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.reduce reducer