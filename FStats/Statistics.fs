namespace FStats

open Utilities

module Statistics =

    /// <summary>Returns the lower quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The lower quartile value.</returns>
    let lowerQuartile (data: seq<float>) =
        let sortedData = Seq.sort data
        let index = getIndex data 1 4.
        let isEven = float (Seq.length data) / 4. |> (fun x -> x % 2. = 0.)
        match isEven with
            | false -> Seq.nth index sortedData
            | true  -> (Seq.nth index sortedData + Seq.nth (index + 1) sortedData) / 2.            
    
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
        let sortedData = Seq.sort data
        let index = getIndex data 1 2.
        let isEven = Seq.length data % 2 = 0
        match isEven with
            | false -> Seq.nth index sortedData
            | true  -> (Seq.nth index sortedData + Seq.nth (index + 1) sortedData) / 2.            

    /// <summary>Returns the minimum value in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The minimum value.</returns>
    let inline minimum (data: seq<float>) = Seq.min data

    /// <summary>Returns the mode of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The mode value(s) list.</returns>
    let inline mode (data: seq<'T>) =
        Seq.groupBy id data
        |> Seq.groupBy (fun (_, x) -> Seq.length x)
        |> Seq.maxBy fst
        |> snd
        |> Seq.map fst
        |> Seq.toList

    /// <summary>Calculates the range of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The range value.</returns>
    let inline range (data : seq<float>) =
        let max = maximum data
        let min = minimum data
        max - min


