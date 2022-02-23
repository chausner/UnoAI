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
           GreedyBot.Factory(true)
           DiversityBot.Factory(true)
           CardRankingBot.Factory(CardRankingBot.RanksWinRate, CardRankingBot.MinCardCountsWinRate, CardRankingBot.PlayDrawnCardThresholdsWinRate, true)
           ScoreBot.Factory(ScoreBot.WeightsWinRateNew)
           CardCountingBot.Factory(CardCountingBot.WeightsWinRate) |]
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
