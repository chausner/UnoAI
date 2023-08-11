module MixBotOptimization

open Game
open BotRunner
open RandomBot
open MixBot
open System.IO

let optimizeMinCardCounts () =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 1_000_000

    let enumerateThresholds n start =
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

    let enumerateThresholdsFromFile path =
        File.ReadAllLines(path)
        |> Seq.map (fun line -> line.Split('\t') |> Array.map int)

    let printThresholds (scoring: int []) =
        String.concat " " (scoring |> Seq.map string)

    let pid = System.Environment.ProcessId
    use output = File.CreateText(@$"C:\Users\chris\Desktop\UnoAI\run6\{pid}.csv")
    output.AutoFlush <- true

    //for thresholds in enumerateThresholds 7 -1 do
    //for thresholds in enumerateThresholdsFromFile @"C:\Users\chris\Desktop\UnoAI\run5\best-winrate.csv" do
    for thresholds in enumerateThresholdsFromFile @"C:\Users\chris\Desktop\UnoAI\run5\best-avgscore.csv" do
        let bots =
            //MixBot.Factory({ MixBot.DefaultSettingsWinRate with MinCardCounts = thresholds }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            MixBot.Factory({ MixBot.DefaultSettingsAvgPoints with MinCardCounts = thresholds }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printThresholds thresholds) winRate averagePoints
        with
        | e -> () //printfn "error"

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

    for thresholds in enumerate [ [-1]; [-1]; [-1]; [-1]; [0;2;3]; [2..4] ] |> Seq.map List.toArray do
        let bots =     
            MixBot.Factory({ MixBot.DefaultSettingsAvgPoints with PlayDrawnCardThresholds = thresholds }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printThresholds thresholds) winRate averagePoints
        with
        | e -> () //printfn "error"