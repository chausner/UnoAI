module MixBotOptimization

open Game
open BotRunner
open RandomBot
open MixBot
open System
open System.IO

let optimizeCardCountLimits () =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 1_000_000

    let enumerateLimits n start =
        seq {
            for t1 = -1 to -1 do
                for t2 = -1 to -1 do
                    for t3 = -1 to -1 do
                        for t4 = -1 to -1 do
                            for t5 = -1 to n do
                                for t6 = -1 to n do
                                    if t1 >= start || t2 >= start || t3 >= start || t4 >= start || t5 >= start || t6 >= start then
                                        yield [| t1; t2; t3; t4; t5; t6 |]
        }

    let enumerateLimitsFromFile path =
        File.ReadAllLines(path)
        |> Seq.map (fun line -> line.Split('\t') |> Array.map int)

    let printLimits (scoring: int []) =
        String.concat " " (scoring |> Seq.map string)

    use output = File.CreateText($"MixBot-CardCountLimits-{Environment.ProcessId}.csv")
    output.AutoFlush <- true

    for limits in enumerateLimits 7 -1 do
    //for limits in enumerateLimitsFromFile @"best-winrate.csv" do
        let bots =
            MixBot.Factory({ MixBot.DefaultSettingsWinRate with CardCountLimits = limits }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printLimits limits) winRate averagePoints
            fprintfn output "%s;%f;%f" (printLimits limits) winRate averagePoints
        with
        | e -> printfn "error"

let optimizePlayDrawnCardThresholds () =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 10_000_000

    let rec enumerate ranges =
        match ranges with
        | [] -> Seq.singleton []
        | range :: tail ->
            seq {
                for e in range do
                    for e' in enumerate tail do
                        yield e :: e'
            }

    let printThresholds (scoring: int []) =
        String.concat " " (scoring |> Seq.map string)

    use output = File.CreateText($"MixBot-PlayDrawnCardThresholds-{Environment.ProcessId}.csv")
    output.AutoFlush <- true

    for thresholds in enumerate [ [-1]; [-1]; [-1]; [-1]; [0;2;3]; [2..4] ] do
        let thresholds = thresholds |> Seq.toArray
        let bots =     
            MixBot.Factory({ MixBot.DefaultSettingsWinRate with PlayDrawnCardThresholds = thresholds }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printThresholds thresholds) winRate averagePoints
            fprintfn output "%s;%f;%f" (printThresholds thresholds) winRate averagePoints
        with
        | e -> printfn "error"