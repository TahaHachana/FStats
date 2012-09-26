namespace FStats

open Types
open Statistics

module DescriptiveStatistics =

    let descriptiveStatistics (data: seq<float>) =
        {
            Maximum = maximum data
            Mean    = mean    data
            Median  = median  data
            Minimum = minimum data
            Mode    = mode    data
        }


