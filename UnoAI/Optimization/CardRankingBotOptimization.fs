module CardRankingBotOptimization

open Game
open BotRunner
open RandomBot
open CardRankingBot
open System
open System.IO

let optimizeRanking () =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 100_000

    let rec getPermutations n : int list seq =
        match n with
        | 0 -> seq { yield [] }
        | _ ->
            let perms = getPermutations (n - 1)
            seq {
                for p in perms do
                    for i = 0 to p.Length do
                        yield (p |> List.truncate i) @ [ n ] @ (p |> List.skip i)
            }

    let printRanking (ranks: int []) =
        String.concat " " (ranks |> Seq.map string)

    use output = File.CreateText($"CardRankingBot-Ranking-{Environment.ProcessId}.csv")
    output.AutoFlush <- true

    for ranks in getPermutations 6 do
        let ranks = ranks |> Seq.toArray
        let bots =     
            CardRankingBot.Factory({ CardRankingBot.DefaultSettingsWinRate with Ranks = ranks }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printRanking ranks) winRate averagePoints
            fprintfn output "%s;%f;%f" (printRanking ranks) winRate averagePoints
        with
        | e -> printfn "error"