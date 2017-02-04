module Utils

open System
open System.Diagnostics
open System.Threading

let (|?) = defaultArg

let stopwatch action =
    let stopwatch = Stopwatch.StartNew()
    let result = action()
    result, stopwatch.Elapsed

let inline (%%) x y =
    ((x % y) + y) % y

let private threadLocalRandom = new ThreadLocal<Random>(fun () -> new Random())

let random() = 
    threadLocalRandom.Value

module Seq =
    let shuffle source =
        let swap (a : 'T []) i j =
            let tmp = a.[i]
            a.[i] <- a.[j]
            a.[j] <- tmp

        let shuffled = source |> Seq.toArray
        let num = Array.length shuffled
        let rand = random()

        for i = 0 to num - 2 do
            let r = rand.Next(i, num)
            swap shuffled i r

        shuffled

    let tryMax source =
        if source |> Seq.isEmpty then
            None
        else
            Some (Seq.max source)

    let tryMaxBy projection source =
        if source |> Seq.isEmpty then
            None
        else
            Some (Seq.maxBy projection source)

module Array =
    let chooseRandom array =
        let length = array |> Array.length
        array.[random().Next(length)]

module List =
    let chooseRandom list =
        let length = list |> List.length
        list |> List.item (random().Next(length))

    let shuffle list =
        list |> Seq.shuffle |> Array.toList