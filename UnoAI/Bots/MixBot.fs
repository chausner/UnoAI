module MixBot

open Card
open Game
open Bot
open Utils
open BotRunner
open RandomBot

type MixBotSettings =
    { Ranks: int []
      MinCardCounts: int []
      PlayDrawnCardThresholds: int []
      ChooseMostCommonColor: bool
      DiversityWeight : float }

/// <summary>
/// Bot that combines the logic of DiversityBot and CardRankingBot.
///
/// This bot achieves a win rate around 0.4 percentage points higher than CardRankingBot.
/// No improvement in average points is observed, however.
/// </summary>
type MixBot(game: Game, player: Player, settings: MixBotSettings) =
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

    let chooseColorIfNeeded card color =
        match card with
        | Wild None
        | WildDrawFour None -> chooseColor card (color ())
        | _ -> card

    let getCardDiversityScore card =
        match card with
        | Wild None
        | WildDrawFour None -> 0
        | _ ->
            game.Players[player]
            |> Seq.except [| Wild None; WildDrawFour None |]
            |> Seq.filter (doCardsMatch card)
            |> Seq.length

    let scoringFunction card = float settings.Ranks[cardTypeIndex card] + float (getCardDiversityScore card) * settings.DiversityWeight

    let getMostCommonColor () =
        game.Players[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let getRandomColor () =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

    let chooseColor () =
        if settings.ChooseMostCommonColor then
            getMostCommonColor () |? getRandomColor ()
        else
            getRandomColor ()

    let pickMaxBy projection list =
        match list with
        | []
        | [ _ ] -> list
        | _ ->
            let maxValue = list |> Seq.map projection |> Seq.max
            list |> List.filter (fun x -> projection x = maxValue)

    override self.PerformAction() =
        let numCardsInHand =
            game.Players[player]
            |> Seq.filter (fun card -> settings.MinCardCounts[cardTypeIndex card] = -1)
            |> Seq.length

        let playableCards =
            game.Players[player]
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard
            |> Seq.filter (fun card -> numCardsInHand <= settings.MinCardCounts[cardTypeIndex card] || settings.MinCardCounts[cardTypeIndex card] = -1)
            |> Seq.toList

        if not (playableCards |> List.isEmpty) then
            let playedCard =
                playableCards
                |> pickMaxBy scoringFunction
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
            DrawCardBotAction (fun drawnCard ->
                let threshold = settings.PlayDrawnCardThresholds[cardTypeIndex drawnCard]
                if game.Players[player].Length <= threshold || threshold = -1 then
                    Some (chooseColorIfNeeded drawnCard chooseColor)
                else
                    None)

    static member Factory(settings: MixBotSettings) =
        fun game player -> new MixBot(game, player, settings) :> Bot

    static member DefaultSettingsWinRate =
        { Ranks = [| 4; 3; 5; 6; 1; 2 |]
          MinCardCounts = [| -1; -1; -1; -1; 2; 4 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true
          DiversityWeight = 1.0 }

    static member DefaultSettingsAvgPoints =
        { Ranks = [| 5; 3; 6; 4; 2; 1 |]
          MinCardCounts = [| -1; -1; -1; -1; 5; 3 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true
          DiversityWeight = 0.5 }

let optimizeDiversityWeight () =
    let ruleSet = defaultRuleSet
    let numPlayers = 3
    let numGames = 1_000_000

    for diversityWeight in [0.0; 0.1; 0.2; 0.5; 1.0; 1.5; 2.0; 3.0; 4.0; 5.0; 10.0; 15.0; 20.0; 50.0; 100.0] do
        let bots =     
            //MixBot.Factory({ MixBot.DefaultSettingsWinRate with DiversityWeight = diversityWeight }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            MixBot.Factory({ MixBot.DefaultSettingsAvgPoints with DiversityWeight = diversityWeight }) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray

        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%f  %.4f %.4f" diversityWeight winRate averagePoints
        with
        | e -> () //printfn "error"