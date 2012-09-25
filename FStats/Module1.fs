namespace FStats

module Utils =

    let inline getIndex (dataset : seq<float>) x y =
        float (Seq.length dataset * x) / y
        |> ceil |> int |> (fun x -> x - 1)

module Statistics =

    let inline mean (dataset : seq<float>) =
        match Seq.isEmpty dataset with
            | false -> Some <| Seq.average dataset
            | true  -> None
        
    let inline median (dataset : seq<float>) =
        match Seq.isEmpty dataset with
            | false ->
                let dataset' = dataset |> Seq.sort
                let index = Utils.getIndex dataset 1 2.
                Some <| Seq.nth index dataset'
            | true -> None