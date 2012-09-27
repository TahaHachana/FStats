namespace FStats

module Types =

    type DescriptiveStatistics =
        {
            InterQuartileRange : float
            LowerQuartile      : float
            Maximum            : float
            Mean               : float
            Median             : float
            Minimum            : float
            Mode               : float list
            PopulationVariance : float
            Range              : float
            UpperQuartile      : float
            Variance           : float
        }


