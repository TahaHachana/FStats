namespace FStats


module Correlation =

    /// <summary>Calculates the covariance between two data sets using (N - 1) degrees of freedom.</summary>
    /// <param name="data">The first data set.</param>
    /// <param name="data'">The second data set.</param>
    /// <returns>The covariance value.</returns>
    let inline covariance data data' =
        let mean = Statistics.mean data
        let mean' = Statistics.mean data'
        let length = Seq.length data - 1 |> float
        Seq.zip data data'
        |> Seq.sumBy (fun (x, y) -> ((x - mean) * (y - mean')) / length)

    /// <summary>Calculates the covariance between two data sets using N degrees of freedom.</summary>
    /// <param name="data">The first data set.</param>
    /// <param name="data'">The second data set.</param>
    /// <returns>The covariance value.</returns>
    let inline populationCovariance data data' =
        let mean = Statistics.mean data
        let mean' = Statistics.mean data'
        Seq.zip data data'
        |> Seq.averageBy (fun (x, y) -> (x - mean) * (y - mean'))

    /// <summary>Calculates the correlation coefficient between two data sets.</summary>
    /// <param name="data">The first data set.</param>
    /// <param name="data'">The second data set.</param>
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



