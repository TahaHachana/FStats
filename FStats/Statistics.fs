namespace FStats

open Utilities

module Statistics =
    
    /// <summary>Returns the maximum value in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The maximum value.</returns>
    let inline maximum (data: seq<float>) = Seq.max data

    /// <summary>Calculates the mean of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The mean value.</returns>
    let inline mean (data: seq<float>) = Seq.average data

    /// <summary>Calculates the median of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The median value.</returns>        
    let inline median (data: seq<float>) =
        let isEven = Seq.length data % 2 = 0
        let data' = Seq.sort data
        let index = getIndex data 1 2.
        match isEven with
            | false -> Seq.nth index data'
            | true  -> (Seq.nth index data' + Seq.nth (index + 1) data') / 2.            

    /// <summary>Returns the minimum value in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The minimum value.</returns>
    let inline minimum (data : seq<float>) = Seq.min data

    /// <summary>Returns the mode of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The mode value(s) list.</returns>
    let inline mode (data : seq<'T>) =
        Seq.groupBy id data
        |> Seq.groupBy (fun (_, x) -> Seq.length x)
        |> Seq.maxBy fst
        |> snd
        |> Seq.map fst
        |> Seq.toList

    /// <summary>Calculates the range of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The range value.</returns>
    let inline range (dataset : seq<float>) =
        let max = maximum dataset
        let min = minimum dataset
        max - min


