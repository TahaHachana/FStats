namespace FStats

open Types
open Statistics

module DescriptiveStatistics =

    let describe (data: seq<float>) =
        let maximum = maximum data
        let minimum = minimum data
        {
            LowerQuartile = lowerQuartile data
            Maximum       = maximum
            Mean          = mean    data
            Median        = median  data
            Minimum       = minimum
            Mode          = mode    data
            Range         = maximum - minimum
        }


