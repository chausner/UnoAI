module Utils

open System
open System.Diagnostics
open System.Runtime.InteropServices

let (|?) = defaultArg

let stopwatch action =
    let stopwatch = Stopwatch.StartNew()
    let result = action stopwatch
    result, stopwatch.Elapsed

let formatTimeSpan (ts: TimeSpan) =
    sprintf "%02d:%02d:%02d.%02d" (int ts.TotalHours) ts.Minutes ts.Seconds (ts.Milliseconds / 10)

let inline (%%) x y = ((x % y) + y) % y

let pickMaxBy projection source =
    match source with
    | []
    | [ _ ] -> source
    | _ ->
        source
        |> Seq.fold (fun (maxValueSoFar, result) x ->
            let value = projection x
            if maxValueSoFar |> Option.isNone then
                Some value, [x]
            elif value < maxValueSoFar.Value then
                maxValueSoFar, result
            elif value = maxValueSoFar.Value then
                maxValueSoFar, x :: result
            else
                Some value, [x]) (None, [])
        |> snd

module Seq =
    let shuffle source =
        let swap (a: 'T []) i j =
            let tmp = a[i]
            a[i] <- a[j]
            a[j] <- tmp

        let shuffled = source |> Seq.toArray
        let num = Array.length shuffled
        let rand = Random.Shared

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
        array[Random.Shared.Next(length)]

module List =
    let chooseRandom list =
        let length = list |> List.length
        list |> List.item (Random.Shared.Next(length))

    let shuffle list =
        list |> Seq.shuffle |> Array.toList

module private NativeMethods =
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool GetConsoleMode(nativeint hConsoleHandle, int& lpMode)

    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool SetConsoleMode(nativeint hConsoleHandle, int dwMode)

    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern nativeint GetStdHandle(uint nStdHandle)

    let STD_OUTPUT_HANDLE = -11 |> uint
    let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004
    let INVALID_HANDLE_VALUE = -1 |> nativeint

let enableVirtualTerminalProcessing () =
    let stdOutHandle = NativeMethods.GetStdHandle(NativeMethods.STD_OUTPUT_HANDLE)
    if stdOutHandle = NativeMethods.INVALID_HANDLE_VALUE || stdOutHandle = 0 then
        failwithf "GetStdHandle failed with error code %i" (Marshal.GetLastWin32Error())
    let mutable consoleMode = 0
    if not (NativeMethods.GetConsoleMode(stdOutHandle, &consoleMode)) then
        failwithf "GetConsoleMode failed with error code %i" (Marshal.GetLastWin32Error())
    let newConsoleMode = consoleMode ||| NativeMethods.ENABLE_VIRTUAL_TERMINAL_PROCESSING
    if not (NativeMethods.SetConsoleMode(stdOutHandle, newConsoleMode)) then
        failwithf "SetConsoleMode failed with error code %i" (Marshal.GetLastWin32Error())
