namespace FStats


module Correlation =

    /// <summary>Calculates the covariance between two data sequences using (N - 1) degrees of freedom.</summary>
    /// <param name="data">The first data sequence.</param>
    /// <param name="data'">The second data sequence.</param>
    /// <returns>The covariance value.</returns>
    let inline covariance data data' =
        let mean = Statistics.mean data
        let mean' = Statistics.mean data'
        let length = Seq.length data - 1 |> float
        Seq.zip data data'
        |> Seq.sumBy (fun (x, y) -> ((x - mean) * (y - mean')) / length)

    /// <summary>Calculates the covariance between two data sequences using N degrees of freedom.</summary>
    /// <param name="data">The first data sequence.</param>
    /// <param name="data'">The second data sequence.</param>
    /// <returns>The covariance value.</returns>
    let inline populationCovariance data data' =
        let mean = Statistics.mean data
        let mean' = Statistics.mean data'
        Seq.zip data data'
        |> Seq.averageBy (fun (x, y) -> (x - mean) * (y - mean'))

    /// <summary>Calculates the correlation coefficient between two data sequences.</summary>
    /// <param name="data">The first data sequence.</param>
    /// <param name="data'">The second data sequence.</param>
    /// <returns>The correlation coefficient value.</returns>
    let inline coefficient data data' =
        let n = Seq.length data |> float
        let sigmaXY = Seq.zip data data' |> Seq.sumBy (fun (x, y) -> x * y)
        let sigmaX = Seq.sum data
        let sigmaY = Seq.sum data'
        let sigmaXCube = data |> Seq.sumBy (fun x -> pown x 2)
        let sigmaYCube = data' |> Seq.sumBy (fun x -> pown x 2)
        (n * sigmaXY - sigmaX * sigmaY)
        /
        sqrt ((n * sigmaXCube - (pown sigmaX 2)) * (n * sigmaYCube - (pown sigmaY 2)))

    /// <summary>Predicts a value in a linear trend (same functionality as Excel's TREND()).</summary>
    /// <param name="data">The first data sequence.</param>
    /// <param name="data'">The second data sequence.</param>
    /// <returns>The predicted value.</returns>
    let inline predict data data' x =
        let mean = Statistics.mean data
        let mean' = Statistics.mean data'
        let stdDev = Statistics.standardDeviation data
        let stdDev' = Statistics.standardDeviation data'
        let coefficient = coefficient data data'
        ((x - mean) / stdDev) * coefficient * stdDev' + mean'


