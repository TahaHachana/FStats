namespace FStats

open Types
open Utilities

module Statistics =

    /// <summary>Returns the inter quartile range of a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The inter quartile range value.</returns>
    let inline interQuartileRange data =
                
        let sortedData = Seq.sort data
        let isEven = Seq.length data |> mod2Eq0

        let lowerQuartile =
            let index = getIndex data 1 4.
            patternMatch isEven index sortedData

        let upperQuartile =
            let index = getIndex data 3 4.
            patternMatch isEven index sortedData

        upperQuartile - lowerQuartile

    /// <summary>Returns the specified quartile in a data sequence.</summary>
    /// <param name="q">The quartile to return.</param>
    /// <param name="data">The data sequence.</param>
    /// <returns>The quartile value.</returns>
    let inline quartile q data =
        match q with
            | Quartile x ->                    
                let sortedData = Seq.sort data
                let index = getIndex data x 4.
                let isEven = Seq.length data |> mod2Eq0
                patternMatch isEven index sortedData
            | _ -> raise <| InvalidQuartileArgument()

    /// <summary>Returns the lower quartile in a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The lower quartile value.</returns>
    let inline lowerQuartile data = quartile 1 data

    /// <summary>Returns the upper quartile in a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The upper quartile value.</returns>
    let inline upperQuartile data = quartile 3 data
            
    /// <summary>Returns the maximum value in a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The maximum value.</returns>
    let inline maximum data = Seq.max data

    /// <summary>Calculates the mean of a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The mean value.</returns>
    let inline mean (data: seq<float>) = Seq.average data

    /// <summary>Calculates the median of a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The median value.</returns>        
    let inline median data = quartile 2 data

    /// <summary>Returns the minimum value in a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The minimum value.</returns>
    let inline minimum data = Seq.min data

    /// <summary>Returns the mode of a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The mode value(s) list.</returns>
    let inline mode data =
        Seq.groupBy id data
        |> Seq.groupBy (fun (_, x) -> Seq.length x)
        |> Seq.maxBy fst
        |> snd
        |> Seq.map fst
        |> Seq.toList

    /// <summary>Returns the specified percentile in a data sequence.</summary>
    /// <param name="p">The percentile to return.</param>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The percentile value.</returns>
    let inline percentile p data =
        match p with
            | Percentile x ->                    
                let sortedData = Seq.sort data
                let index = getIndex data x 100.
                let isEven = Seq.length data |> mod2Eq0
                patternMatch isEven index sortedData
            | _ -> raise <| InvalidPercentileArgument()

    /// <summary>Calculates the range of a data sequence.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The range value.</returns>
    let inline range data = maximum data - minimum data

    /// <summary>Calculates the unbiased variance of a data sequence using (N - 1) degrees of freedom.</summary>
    /// <param name="data">The dat sequence.</param>
    /// <returns>The variance value.</returns>
    let inline variance data =
        let mean = mean data
        data
        |> Seq.sumBy (fun x -> pown (x - mean) 2)
        |> (fun x -> x / (Seq.length data - 1 |> float))

    /// <summary>Calculates the unbiased standard deviation of a data sequence using (N - 1) degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The standard deviation value.</returns>
    let inline standardDeviation data = sqrt <| variance data

    /// <summary>Calculates the standard score (z-score) of a value.</summary>
    /// <param name="x">The value for which to calculate the score.</param>
    /// <param name="mean">The mean of the data.</param>
    /// <param name="stdDev">The standard deviation of the data.</param>
    /// <returns>The z-score value.</returns>
    let inline zScore x mean stdDev = (x - mean) / stdDev