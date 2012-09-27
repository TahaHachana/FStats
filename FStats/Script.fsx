#r @"..\FStats\bin\Debug\FStats.dll"

open System
open FStats

let rand = Random()

let data =
    [ for x in 0 .. 1000000 do
        yield rand.NextDouble() * 10.
    ]

let stats = DescriptiveStatistics.describe data

let maximum   = stats.Maximum
let maximum'  = Statistics.maximum data
let maximum'' = Seq.maximum data

let mean   = stats.Mean
let mean'  = Statistics.mean data
let mean'' = Seq.mean data

let median   = stats.Median
let median'  = Statistics.median data
let median'' = Seq.median data

let minimum   = stats.Minimum
let minimum'  = Statistics.minimum data
let minimum'' = Seq.minimum data

let mode   = stats.Mode
let mode'  = Statistics.mode data
let mode'' = Seq.mode data

let range   = stats.Range
let range'  = Statistics.range data
let range'' = Seq.range data

