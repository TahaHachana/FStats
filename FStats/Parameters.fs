namespace FStats

open Statistics

module Parameters =

    
    /// <summary>Calculates the variance of a data sequence using N degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The variance value.</returns>
    let inline populationVariance data =
        let mean = mean data
        Seq.averageBy (fun x -> pown (x - mean) 2) data

    /// <summary>Calculates the standard deviation of a data sequence using N degrees of freedom.</summary>
    /// <param name="data">The dat set.</param>
    /// <returns>The standard deviation value.</returns>
    let inline populationStandardDeviation data = sqrt <| populationVariance data

