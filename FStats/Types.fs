namespace FStats

open System

module Types =

    type DescriptiveStatistics =
        {
            InterQuartileRange          : float
            LowerQuartile               : float
            Maximum                     : float
            Mean                        : float
            Median                      : float
            Minimum                     : float
            Mode                        : float list
            PopulationStandardDeviation : float
            PopulationVariance          : float
            Range                       : float
            StandardDeviation           : float
            UpperQuartile               : float
            Variance                    : float
        }

    type InvalidQuartileArgument() =
        
        inherit Exception()

        override x.Message = "Invalid quartile argument, possible values are 1, 2 or 3."
