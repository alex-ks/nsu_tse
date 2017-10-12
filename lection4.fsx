let rec fib () = 
    seq {
        yield 0
        yield 1
        for v1, v2 in Seq.pairwise (fib ()) do
            yield v1 + v2
    }

module Seq = 
    let append' s1 s2 = seq {
        for v in s1 -> v
        for v in s2 -> v
    }   

// doesnt work
let rec fib' () = 
    seq { yield 0; yield 1; }
    |> Seq.append' (fib' () |> Seq.pairwise |> Seq.map (fun (v1, v2) -> v1 + v2)) 

let f = fib' ()

// f
// |> Seq.take 7
// |> Seq.map string
// |> String.concat " "
// |> printfn "%s"
