namespace FStats

open Types
open Statistics

module DescriptiveStatistics =

    let describe (data: seq<float>) =

        let lowerQuartile = lowerQuartile data
        let maximum = maximum data
        let minimum = minimum data
        let populationVariance = populationVariance data
        let upperQuartile = upperQuartile data
        let variance = variance data

        {
            InterQuartileRange          = upperQuartile - lowerQuartile
            LowerQuartile               = lowerQuartile
            Maximum                     = maximum
            Mean                        = mean data
            Median                      = median data
            Minimum                     = minimum
            Mode                        = mode data
            PopulationStandardDeviation = sqrt populationVariance
            PopulationVariance          = populationVariance
            Range                       = maximum - minimum
            StandardDeviation           = sqrt variance
            UpperQuartile               = upperQuartile
            Variance                    = variance
        }


