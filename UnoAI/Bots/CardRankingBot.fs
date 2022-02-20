module CardRankingBot

open Card
open Game
open Bot
open Utils
open BotRunner
open RandomBot
open System.IO

/// <summary>
/// Bot that chooses cards based on a fixed ranking.
///
/// * Out of all playable cards, the card that has the highest rank is chosen.
/// * When a drawn card can be played, it is always played.
/// * When a Wild or WildDrawFour is played, the color that is most common in the player's hand is chosen.
/// </summary>
type CardRankingBot(game : Game, player : Player, ranks : int [], minCardCounts : int [], playDrawnCardThresholds : int [], chooseMostCommonColor : bool) =
    inherit Bot()

    let cardTypeIndex card = 
        match card with
        | StandardCard (_, _) -> 0
        | Reverse _ when game.RuleSet.TwoPlayerReverseIsSkip && game.NumPlayers = 2 -> 2
        | Reverse _           -> 1
        | Skip _              -> 2
        | DrawTwo _           -> 3
        | Wild _              -> 4
        | WildDrawFour _      -> 5

    let scoringFunction card = ranks[cardTypeIndex card]

    let chooseColorIfNeeded card color =
        match card with
        | Wild None         
        | WildDrawFour None -> chooseColor card (color())
        | _                 -> card

    let getMostCommonColor() =
        game.Players[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let getRandomColor() =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

    let chooseColor() =
        if chooseMostCommonColor then
            getMostCommonColor() |? getRandomColor()
        else
            getRandomColor()

    let pickMaxBy projection list =
        match list with
        | [] | [_] -> list
        | _        ->
            let maxValue = list |> Seq.map projection |> Seq.max
            list |> List.filter (fun x -> projection x = maxValue)

    override self.PerformAction() =
        let numCardsInHand =
            game.Players[player]
            |> Seq.filter (fun card -> minCardCounts[cardTypeIndex card] = -1)
            |> Seq.length

        let playableCards =            
            game.Players[player]  
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard
            |> Seq.filter (fun card -> numCardsInHand <= minCardCounts[cardTypeIndex card] || minCardCounts[cardTypeIndex card] = -1)
            |> Seq.toList

        if not (playableCards |> List.isEmpty) then
            let playedCard =
                playableCards
                |> pickMaxBy scoringFunction
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
//            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard chooseColor))
            DrawCardBotAction (fun drawnCard -> 
                let threshold = playDrawnCardThresholds[cardTypeIndex drawnCard]
                if game.Players[player].Length <= threshold || threshold = -1 then
                    Some (chooseColorIfNeeded drawnCard chooseColor)
                else
                    None)

    static member Factory(ranks : int [], minCardCounts : int [], playDrawnCardThresholds : int [], chooseMostCommonColor : bool) =
        fun (game, player) -> new CardRankingBot(game, player, ranks, minCardCounts, playDrawnCardThresholds, chooseMostCommonColor) :> Bot

    static member RanksWinRate   = [| 4; 3; 5; 6; 1; 2 |] // optimal ranking to optimize win rate against 3 other random bots (based on 10,000,000 games)
    static member RanksAvgPoints = [| 5; 3; 6; 4; 2; 1 |] // optimal ranking to optimize average points against 3 other random bots (based on 10,000,000 games)

    static member MinCardCountsWinRate   = [| -1; -1; -1; -1; 2; 4 |]
    static member MinCardCountsAvgPoints = [| -1; -1; -1; -1; 5; 3 |]

    static member PlayDrawnCardThresholdsWinRate   = [| -1; -1; -1; -1; 2; 2 |]
    static member PlayDrawnCardThresholdsAvgPoints = [| -1; -1; -1; -1; 2; 2 |]

let optimizeRanking() =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 10_000_0

    let rec getPermutations n : int list seq =
        match n with
        | 0 -> seq { yield [ ] }
        | _ -> let perms = getPermutations (n - 1)
               seq {
                   for p in perms do
                       for i = 0 to p.Length do
                           yield (p |> List.truncate i) @ [ n ] @ (p |> List.skip i) }

    let printRanking (scoring : int []) =
        String.concat " " (scoring |> Seq.map string)

    for ranks in getPermutations 6 do
        let ranks = ranks |> Seq.toArray
        let bots =     
            CardRankingBot.Factory(ranks, CardRankingBot.MinCardCountsWinRate, CardRankingBot.PlayDrawnCardThresholdsWinRate, true) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printRanking ranks) winRate averagePoints
        with
        | e -> () //printfn "error"

let optimizeMinCardCounts() =
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
                                        yield [| t1; t2; t3; t4; t5; t6 |] }

    let enumerateThresholdsFromFile path =
        File.ReadAllLines(path)
        |> Seq.map (fun line -> line.Split('\t') |> Array.map int)

    let printThresholds (scoring : int []) =
        String.concat " " (scoring |> Seq.map string)

    let pid = System.Environment.ProcessId
    use output = File.CreateText(@$"C:\Users\chris\Desktop\UnoAI\run6\{pid}.csv")
    output.AutoFlush <- true

    //for thresholds in enumerateThresholds 7 -1 do
    //for thresholds in enumerateThresholdsFromFile @"C:\Users\chris\Desktop\UnoAI\run5\best-winrate.csv" do
    for thresholds in enumerateThresholdsFromFile @"C:\Users\chris\Desktop\UnoAI\run5\best-avgscore.csv" do
        let bots =     
            //CardRankingBot.Factory(CardRankingBot.RanksWinRate, thresholds, CardRankingBot.PlayDrawnCardThresholdsWinRate, true) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            CardRankingBot.Factory(CardRankingBot.RanksAvgPoints, thresholds, CardRankingBot.PlayDrawnCardThresholdsAvgPoints, true) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
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

let optimizePlayDrawnCardThresholds() =
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

    let printThresholds (scoring : int []) =
        String.concat " " (scoring |> Seq.map string)

    for thresholds in enumerate [ [-1]; [-1]; [-1]; [-1]; [0;2;3]; [2..4] ] |> Seq.map List.toArray do
        let bots =     
            CardRankingBot.Factory(CardRankingBot.RanksAvgPoints, CardRankingBot.MinCardCountsAvgPoints, thresholds, true) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
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