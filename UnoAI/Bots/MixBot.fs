module MixBot

open Card
open Game
open Bot
open Utils

type MixBotSettings =
    { Ranks: int []
      PrioritizeMostCommonColor: bool
      CardCountLimits: int []
      PlayDrawnCardThresholds: int []
      ChooseMostCommonColor: bool }

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

    let getRandomColor () =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

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
            |> Seq.filter (fun card -> numCardsInHand <= settings.CardCountLimits[cardTypeIndex card] || settings.CardCountLimits[cardTypeIndex card] = -1)
            |> Seq.toList

        if not (playableCards |> List.isEmpty) then
            let candidates =
                if settings.PrioritizeMostCommonColor then
                    let mostCommonColors = getMostCommonColors ()
                    let playableCardsWithMostCommonColor = 
                        playableCards |> List.filter (fun card ->
                            match getCardColor card with
                            | Some color when mostCommonColors |> List.contains color -> true
                            | _ -> false)
                    match playableCardsWithMostCommonColor with
                    | [] -> playableCards
                    | _  -> playableCardsWithMostCommonColor
                else
                    playableCards
            let playedCard =
                candidates
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
          PrioritizeMostCommonColor = true
          CardCountLimits = [| -1; -1; -1; -1; 2; 4 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true }

    static member DefaultSettingsAvgPoints =
        { Ranks = [| 5; 3; 6; 4; 2; 1 |]
          PrioritizeMostCommonColor = true
          CardCountLimits = [| -1; -1; -1; -1; 5; 3 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true }
