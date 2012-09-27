namespace FStats

module internal Utilities =
    
    let inline getIndex (dataset : seq<float>) x y =
        float (Seq.length dataset * x) / y
        |> ceil
        |> int
        |> (fun x -> x - 1)

    let inline patternMatch isEven index data =
        match isEven with
            | false -> Seq.nth index data
            | true  -> (Seq.nth index data + Seq.nth (index + 1) data) / 2.            
