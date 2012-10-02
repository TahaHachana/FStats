namespace FStats

module internal Utilities =
    
    let inline getIndex (dataset : seq<float>) x y =
        float (Seq.length dataset * x) / y
        |> ceil
        |> int
        |> (fun x -> x - 1)

    let inline mod2Eq0 x = x % 2 = 0

    let inline patternMatch isEven index data =
        match isEven with
            | false -> Seq.nth index data
            | true  -> (Seq.nth index data + Seq.nth (index + 1) data) / 2.        

    let inline (|Percentile|_|) input =
        match List.exists (fun x -> x = input) [1 .. 99] with
            | false -> None
            | true  -> Some input

    let inline (|Quartile|_|) input =
        match List.exists (fun x -> x = input) [1 .. 3] with
            | false -> None
            | true  -> Some input
                
