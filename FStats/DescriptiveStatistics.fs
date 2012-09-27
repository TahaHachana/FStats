namespace FStats

open Types
open Statistics

module DescriptiveStatistics =

    let describe (data: seq<float>) =
        let maximum = maximum data
        let minimum = minimum data
        let lowerQuartile = lowerQuartile data
        let upperQuartile = upperQuartile data
        {
            InterQuartileRange = upperQuartile - lowerQuartile
            LowerQuartile      = lowerQuartile
            Maximum            = maximum
            Mean               = mean data
            Median             = median data
            Minimum            = minimum
            Mode               = mode data
            PopulationVariance = populationVariance data
            Range              = maximum - minimum
            UpperQuartile      = upperQuartile
            Variance           = variance data
        }


