module MixBot

open Card
open Game
open Bot
open Utils
open BotUtils

type MixBotSettings =
    { Ranks: int []
      EndGameRanks: int []
      PrioritizeMostCommonColor: bool
      CardCountLimits: int []
      PlayDrawnCardThresholds: int []
      ChooseMostCommonColor: bool
      EnableEndGameLogic: bool }

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

    let scoringFunction card = settings.Ranks[cardTypeIndex card]

    let endGameScoringFunction card = settings.EndGameRanks[cardTypeIndex card]

    let isEndGame () =
        let nextPlayer = (game.ActivePlayer + (if game.Direction = Clockwise then 1 else -1)) %% game.NumPlayers
        let nextPlayerCardCount = game.Players[nextPlayer] |> List.length
        nextPlayerCardCount = 1

    let getCardDiversityScore card =
        match card with
        | Wild None
        | WildDrawFour None -> 0
        | _ ->
            game.Players[player]
            |> Seq.except [| Wild None; WildDrawFour None |]
            |> Seq.filter (doCardsMatch card)
            |> Seq.length

    let getMostCommonColor () =
        game.Players[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let getMostCommonColors () =
        game.Players[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.pickMaxBy snd
        |> List.map fst

    let chooseColor () =
        if settings.ChooseMostCommonColor then
            getMostCommonColor () |? getRandomColor ()
        else
            getRandomColor ()

    override self.PerformAction() =
        let numCardsInHand =
            game.Players[player]
            |> Seq.filter (fun card -> settings.CardCountLimits[cardTypeIndex card] = -1)
            |> Seq.length

        let playableCards =
            game.Players[player]
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard
            |> Seq.toList

        if settings.EnableEndGameLogic && isEndGame () && not (playableCards |> List.isEmpty) then
            let playedCard =
                playableCards
                |> Seq.pickMaxBy endGameScoringFunction
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
            let playableCardsWithinLimits =
                playableCards
                |> Seq.filter (fun card -> 
                    let limit = settings.CardCountLimits[cardTypeIndex card]
                    numCardsInHand <= limit || limit = -1)
                |> Seq.toList

            if not (playableCardsWithinLimits |> List.isEmpty) then
                let mostCommonColors = getMostCommonColors ()
                let playedCard =
                    playableCardsWithinLimits
                    |> (if settings.PrioritizeMostCommonColor then
                            Seq.pickMaxBy (fun card -> 
                                match getCardColor card with
                                | Some color when mostCommonColors |> List.contains color -> 1
                                | _ -> 0)
                        else
                            id)
                    |> Seq.pickMaxBy scoringFunction
                    |> Seq.pickMaxBy getCardDiversityScore
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
          EndGameRanks = [| 2; 3; 4; 6; 1; 5 |]
          PrioritizeMostCommonColor = true
          CardCountLimits = [| -1; -1; -1; -1; 2; 4 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true
          EnableEndGameLogic = true }

    static member DefaultSettingsAvgPoints =
        { Ranks = [| 5; 3; 6; 4; 2; 1 |]
          EndGameRanks = [| 2; 3; 4; 6; 1; 5 |]
          PrioritizeMostCommonColor = true
          CardCountLimits = [| -1; -1; -1; -1; 5; 3 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true 
          EnableEndGameLogic = true }
