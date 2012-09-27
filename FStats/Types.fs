namespace FStats

module Types =

    type DescriptiveStatistics =
        {
            LowerQuartile : float
            Maximum       : float
            Mean          : float
            Median        : float
            Minimum       : float
            Mode          : float list
            Range         : float
        }


