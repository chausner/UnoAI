open Game
open BotRunner
open RandomBot
open GreedyBot
open DiversityBot
open CardRankingBot
open MixBot
open MixBot2
open ScoreBot
open CardCountingBot

let runStatsOneVsRandomBots numPlayers =
    let ruleSet = defaultRuleSet
    let bots =
        [| RandomBot.Factory()
           GreedyBot.Factory(GreedyBot.DefaultSettings)
           DiversityBot.Factory(DiversityBot.DefaultSettings)
           CardRankingBot.Factory(CardRankingBot.DefaultSettingsWinRate)
           CardRankingBot.Factory(CardRankingBot.DefaultSettingsAvgPoints)
           MixBot.Factory(MixBot.DefaultSettingsWinRate)
           MixBot.Factory(MixBot.DefaultSettingsAvgPoints)
           MixBot2.Factory(MixBot2.DefaultSettingsWinRate)
           MixBot2.Factory(MixBot2.DefaultSettingsAvgPoints)
           ScoreBot.Factory(ScoreBot.DefaultSettingsWinRate)
           ScoreBot.Factory(ScoreBot.DefaultSettingsAvgPoints)
           CardCountingBot.Factory(CardCountingBot.DefaultSettingsWinRate)
           CardCountingBot.Factory(CardCountingBot.DefaultSettingsAvgPoints) |]
    let randomizePlayOrder = true
    let timeout = 1000
    let numGames = 10_000_000
    let bot2 = RandomBot.Factory()

    printfn "%d players, * vs. %d RandomBot(s)" numPlayers (numPlayers - 1)
    printfn "%-18s %10s %13s" "Bot" "Win rate" "Avg. points"

    for bot1 in bots do
        let stats = initStats numPlayers
        let bots' = bot1 :: (List.replicate (numPlayers - 1) bot2) |> List.toArray
        runBotsBatch ruleSet bots' randomizePlayOrder timeout numGames
        |> Seq.iter (fun result -> updateStats stats result)
        let botName1 = (bot1 (new Game.Game(defaultRuleSet, 2, 0)) 0).GetType().Name
        let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
        let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)
        printfn "%-18s %10.3f %13.1f" botName1 winRate averagePoints

    printfn ""

[<EntryPoint>]
let main argv =
    let ruleSet = defaultRuleSet
    let bots =
        [| RandomBot.Factory()
           GreedyBot.Factory(GreedyBot.DefaultSettings)
           DiversityBot.Factory(DiversityBot.DefaultSettings)
           CardRankingBot.Factory(CardRankingBot.DefaultSettingsWinRate)
           MixBot.Factory(MixBot.DefaultSettingsWinRate)
           MixBot2.Factory(MixBot2.DefaultSettingsWinRate)
           ScoreBot.Factory(ScoreBot.DefaultSettingsWinRate)
           CardCountingBot.Factory(CardCountingBot.DefaultSettingsWinRate) |]
    let randomizePlayOrder = true
    let timeout = 1000
    let numGames = 1_000_000

    runBatchAndPrintStats ruleSet bots randomizePlayOrder timeout numGames

    //CardRankingBotOptimization.optimizeRanking()
    //CardRankingBotOptimization.optimizeCardCountLimits()
    //CardRankingBotOptimization.optimizePlayDrawnCardThresholds()

    //MixBotOptimization.optimizeCardCountLimits()
    //MixBotOptimization.optimizePlayDrawnCardThresholds()

    //MixBot2Optimization.optimizeEndGameCardCountLimits()
    //MixBot2Optimization.optimizeWildPreferredColorSlack()

    //ScoreBotOptimization.optimizeWeights()

    //CardCountingBotOptimization.optimizeWeights()

    // generate statistics for README.md
    //runStatsOneVsRandomBots 2
    //runStatsOneVsRandomBots 6

    0