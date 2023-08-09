module CardCountingBotOptimization

open Game
open BotRunner
open RandomBot
open CardCountingBot
open System
open System.Globalization
open System.IO

let optimizeWeights () =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 100_000

    let printScoring (scoring: float []) =
        String.concat " " (scoring |> Seq.map (fun x -> x.ToString("F3", CultureInfo.InvariantCulture).PadLeft(6)))

    let ranges = [ (-0.26, -0.17); (-0.26, -0.17); (-0.22, -0.08); (-0.32, -0.10); (-0.35, -0.20); (-0.13, 0.14); (-0.12, 0.08) ]

    let getRandomInRanges ranges =
        ranges
        |> Seq.map (fun (l, h) -> Random.Shared.NextDouble() * (h - l) + l)
        |> Seq.toArray

    let getRandomNear scoring margin =
        scoring |> Array.map (fun x -> x * (1.0 + Random.Shared.NextDouble() * 2.0 * margin - margin))

    let normalizeScoring scoring =
        let s = scoring |> Seq.map abs |> Seq.sum
        scoring |> Array.map (fun x -> x / s)

    let pid = System.Environment.ProcessId
    use output = File.CreateText(@$"C:\Users\chris\Desktop\UnoAI\run5\{pid}.csv")
    output.AutoFlush <- true

    //for scoring in Seq.initInfinite (fun _ -> Array.init 6 (fun _ -> Random.Shared.NextDouble() * 2.0 - 1.0)) do
    //for scoring in Seq.initInfinite (fun _ -> getRandomInRanges ranges) |> Seq.map normalizeScoring do
    //for scoring in Seq.initInfinite (fun _ -> getRandomNear [| -0.359;0.431;0.041;0.062;0.108 |] 0.2) |> Seq.map normalizeScoring do
    //for scoring in [0.00001..0.01..0.2] |> Seq.map (fun w -> Array.append scoring' [| w |]) do
    for param in [ 1..40 ] |> Seq.map (fun p -> -0.0001 * float p) do
        //let scoring = scoring |> Seq.toArray
        let scoring = CardCountingBot.DefaultSettingsWinRate.Weights
        scoring[6] <- param

        let bots =     
            CardCountingBot.Factory({ Weights = scoring }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray
            
        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printScoring scoring) winRate averagePoints
            output.WriteLine(String.Join(';', (Array.append scoring [| winRate; averagePoints |])))
            
        with
        | e -> printfn "error"