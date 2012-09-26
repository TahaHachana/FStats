namespace FStats

module internal Utilities =
    
    let inline getIndex (dataset : seq<float>) x y =
        float (Seq.length dataset * x) / y
        |> ceil
        |> int
        |> (fun x -> x - 1)

