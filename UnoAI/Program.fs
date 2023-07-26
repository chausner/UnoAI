open Game
open BotRunner
open RandomBot
open GreedyBot
open CardRankingBot
open MixBot
open DiversityBot
open ScoreBot
open CardCountingBot

[<EntryPoint>]
let main argv =
    let ruleSet = defaultRuleSet
    let bots =
        [| RandomBot.Factory()
           RandomBot.Factory()
           //GreedyBot.Factory(GreedyBot.DefaultSettings)
           //DiversityBot.Factory(DiversityBot.DefaultSettings)
           //CardRankingBot.Factory(CardRankingBot.DefaultSettingsWinRate)
           MixBot.Factory(MixBot.DefaultSettingsWinRate)
           //ScoreBot.Factory(ScoreBot.DefaultSettingsWinRateNew)
           //CardCountingBot.Factory(CardCountingBot.DefaultSettingsWinRate) |]
           |]
    let randomizePlayOrder = true
    let timeout = 1000
    let numGames = 1_000_000

    runBatchAndPrintStats ruleSet bots randomizePlayOrder timeout numGames

    //CardRankingBot.optimizeRanking()
    //CardRankingBot.optimizeMinCardCounts()
    //CardRankingBot.optimizePlayDrawnCardThresholds()

    //ScoreBot.optimizeWeights()

    //CardCountingBot.optimizeWeights()

    0
