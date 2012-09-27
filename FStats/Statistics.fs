namespace FStats

open Utilities

module Statistics =

    /// <summary>Returns the lower quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The lower quartile value.</returns>
    let inline interQuartileRange (data: seq<float>) =
        
        let sortedData = Seq.sort data
        
        let lowerQuartile =
            let index = getIndex data 1 4.
            let isEven = float (Seq.length data) / 4. |> (fun x -> x % 2. = 0.)
            patternMatch isEven index sortedData

        let upperQuartile =
            let index = getIndex data 3 4.
            let isEven = float (Seq.length data * 3) / 4. |> (fun x -> x % 2. = 0.)
            patternMatch isEven index sortedData

        upperQuartile - lowerQuartile

    /// <summary>Returns the lower quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The lower quartile value.</returns>
    let inline lowerQuartile (data: seq<float>) =
        let sortedData = Seq.sort data
        let index = getIndex data 1 4.
        let isEven = float (Seq.length data) / 4. |> (fun x -> x % 2. = 0.)
        patternMatch isEven index sortedData
            
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
        patternMatch isEven index sortedData

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

    /// <summary>Calculates the variance of a data set using N degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The variance value.</returns>
    let inline populationVariance (data: seq<float>) =
        let mean = mean data
        data
        |> Seq.map (fun x -> pown x 2)
        |> Seq.average
        |> (fun x -> x - (pown mean 2))

    /// <summary>Calculates the range of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The range value.</returns>
    let inline range (data : seq<float>) =
        let max = maximum data
        let min = minimum data
        max - min

    /// <summary>Returns the upper quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The upper quartile value.</returns>
    let inline upperQuartile (data: seq<float>) =
        let sortedData = Seq.sort data
        let index = getIndex data 3 4.
        let isEven = float (Seq.length data * 3) / 4. |> (fun x -> x % 2. = 0.)
        patternMatch isEven index sortedData

    /// <summary>Calculates the variance of a data set using (N - 1) degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The variance value.</returns>
    let inline variance (data: seq<float>) =
        let mean = mean data
        data
        |> Seq.map (fun x -> pown x 2)
        |> Seq.sum
        |> (fun x -> x / (Seq.length data - 1 |> float))
        |> (fun x -> x - (pown mean 2))
