namespace FStats

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


