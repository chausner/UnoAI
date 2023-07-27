open Game
open BotRunner
open RandomBot
open GreedyBot
open CardRankingBot
open DiversityBot
open ScoreBot
open CardCountingBot

[<EntryPoint>]
let main argv =
    let ruleSet = defaultRuleSet
    let bots =
        [| RandomBot.Factory()
           GreedyBot.Factory(GreedyBot.DefaultSettings)
           DiversityBot.Factory(DiversityBot.DefaultSettings)
           CardRankingBot.Factory(CardRankingBot.DefaultSettingsWinRate)
           ScoreBot.Factory(ScoreBot.DefaultSettingsWinRate)
           CardCountingBot.Factory(CardCountingBot.DefaultSettingsWinRate) |]
    let randomizePlayOrder = true
    let timeout = 1000
    let numGames = 1_000_000

    runBatchAndPrintStats ruleSet bots randomizePlayOrder timeout numGames

    //CardRankingBotOptimization.optimizeRanking()
    //CardRankingBotOptimization.optimizeMinCardCounts()
    //CardRankingBotOptimization.optimizePlayDrawnCardThresholds()

    //ScoreBotOptimization.optimizeWeights()

    //CardCountingBotOptimization.optimizeWeights()

    0
