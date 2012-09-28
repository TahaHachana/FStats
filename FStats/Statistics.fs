namespace FStats

open Types
open Utilities

module Statistics =

    /// <summary>Returns the lower quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The lower quartile value.</returns>
    let inline interQuartileRange (data: seq<float>) =
        
        let sortedData = Seq.sort data
        
        let lowerQuartile =
            let index = getIndex data 1 4.
            let isEven = float (Seq.length data) / 4. |> mod2Eq0
            patternMatch isEven index sortedData

        let upperQuartile =
            let index = getIndex data 3 4.
            let isEven = float (Seq.length data * 3) / 4. |> mod2Eq0
            patternMatch isEven index sortedData

        upperQuartile - lowerQuartile

    /// <summary>Returns the lower quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The lower quartile value.</returns>
    let inline lowerQuartile (data: seq<float>) =
        let sortedData = Seq.sort data
        let index = getIndex data 1 4.
        let isEven = float (Seq.length data) / 4. |> mod2Eq0
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

    /// <summary>Returns the specified percentile in a data set.</summary>
    /// <param name="p">The percentile to return.</param>
    /// <param name="data">The dat set.</param>
    /// <returns>The percentile value.</returns>
    let inline percentile p (data: seq<float>) =
        match p with
            | Percentile x ->                    
                let sortedData = Seq.sort data
                let index = getIndex data x 100.
                let isEven = float (Seq.length data) / 100. |> mod2Eq0
                patternMatch isEven index sortedData
            | _ -> raise <| InvalidPercentileArgument()

    /// <summary>Calculates the standard deviation of a data set using N degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The standard deviation value.</returns>
    let inline populationStandardDeviation data =
        let mean = mean data
        data
        |> Seq.map (fun x -> pown x 2)
        |> Seq.average
        |> (fun x -> x - (pown mean 2))
        |> sqrt
    
    /// <summary>Calculates the variance of a data set using N degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The variance value.</returns>
    let inline populationVariance (data: seq<float>) =
        let mean = mean data
        data
        |> Seq.map (fun x -> pown x 2)
        |> Seq.average
        |> (fun x -> x - (pown mean 2))

    /// <summary>Returns the specified quartile in a data set.</summary>
    /// <param name="q">The quartile to return.</param>
    /// <param name="data">The dat set.</param>
    /// <returns>The quartile value.</returns>
    let inline quartile q (data: seq<float>) =
        match q with
            | Quartile x ->                    
                let sortedData = Seq.sort data
                let index = getIndex data x 4.
                let isEven = float (Seq.length data) / 4. |> mod2Eq0
                patternMatch isEven index sortedData
            | _ -> raise <| InvalidQuartileArgument()

    /// <summary>Calculates the range of a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The range value.</returns>
    let inline range (data : seq<float>) =
        let max = maximum data
        let min = minimum data
        max - min

    /// <summary>Calculates the standard deviation of a data set using (N - 1) degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The standard deviation value.</returns>
    let inline standardDeviation data =
        let mean = mean data
        data
        |> Seq.map (fun x -> pown x 2)
        |> Seq.sum
        |> (fun x -> x / (Seq.length data - 1 |> float))
        |> (fun x -> x - (pown mean 2))
        |> sqrt

    /// <summary>Returns the upper quartile in a data set.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The upper quartile value.</returns>
    let inline upperQuartile (data: seq<float>) =
        let sortedData = Seq.sort data
        let index = getIndex data 3 4.
        let isEven = float (Seq.length data * 3) / 4. |> mod2Eq0
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

    /// <summary>Calculates the standard score (z-score) of a value in a data set.</summary>
    /// <param name="x">The value for which to calculate the score.</param>
    /// <param name="data">The dat set.</param>
    /// <returns>The z-score value.</returns>
    let inline zScore x data =
        let mean = mean data
        
        let stdDev =
            data
            |> Seq.map (fun x -> pown x 2)
            |> Seq.sum
            |> (fun x -> x / (Seq.length data - 1 |> float))
            |> (fun x -> x - (pown mean 2))
            |> sqrt

        (x - mean) / stdDev
