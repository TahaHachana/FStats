#load "Module1.fs"

open System
open FStats

let rand = Random()

let dataset =
    [ for x in 0 .. 100000 do
        yield rand.NextDouble() * 10.
    ]

let mean = Statistics.mean dataset
let median = Statistics.median dataset
