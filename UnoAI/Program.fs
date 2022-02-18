open Utils
open Game
open BotRunner
open RandomBot
open GreedyBot
open CardRankingBot
open DiversityBot
open ScoreBot
open CardCountingBot
open System
open System.Diagnostics

let runBatchAndPrintStats() =
    let ruleSet = defaultRuleSet
    let numGames = 1_000_000
    let randomizePlayOrder = true
    let timeout = 1000
    let bots = [|
        RandomBot.Factory();
        GreedyBot.Factory(true);
        DiversityBot.Factory(true)
        CardRankingBot.Factory(CardRankingBot.RanksWinRate, CardRankingBot.MinCardCountsWinRate, CardRankingBot.PlayDrawnCardThresholdsWinRate, true);
        ScoreBot.Factory(ScoreBot.WeightsWinRateNew);
        CardCountingBot.Factory(CardCountingBot.WeightsWinRate);
    |] 

    printfn "Games: %i" numGames
    printfn "Game timeout: %i" timeout

    let cursorPos = Console.GetCursorPosition()
    let restoreCursor() =
        let struct (cursorLeft, cursorTop) = cursorPos
        Console.SetCursorPosition(cursorLeft, cursorTop)

    let stats = initStats bots.Length
    let mutable lastProgressUpdate: TimeSpan option = None

    let printProgress (stopwatch : Stopwatch) =
        if lastProgressUpdate.IsNone || (stopwatch.Elapsed - lastProgressUpdate.Value).TotalSeconds >= 1.0 then
            printfn "Progress: %.0f%%" ((float stats.NumGames) / (float numGames) * 100.0)
            let eta = 
                match stats.NumGames with
                | 0 -> TimeSpan.Zero
                | _ -> float (numGames - stats.NumGames) / float stats.NumGames * stopwatch.Elapsed            
            printfn "Elapsed: %s (ETA: %s)" (stopwatch.Elapsed |> formatTimeSpan) (eta |> formatTimeSpan)
            printStats stats bots
            restoreCursor()
            lastProgressUpdate <- Some stopwatch.Elapsed

    let _, elapsed = stopwatch (fun stopwatch ->
        runBotsBatch ruleSet bots randomizePlayOrder timeout numGames
        |> Seq.iter (fun result ->
            updateStats stats result
            printProgress stopwatch))

    printfn "Progress: 100%%"
    printfn "Elapsed: %s" (elapsed |> formatTimeSpan)
    printStats stats bots

[<EntryPoint>]
let main argv = 
    runBatchAndPrintStats()

    //CardRankingBot.optimizeRanking()
    //CardRankingBot.optimizeMinCardCounts()
    //CardRankingBot.optimizePlayDrawnCardThresholds()

    //ScoreBot.optimizeWeights()

    //CardCountingBot.optimizeWeights()

    0
