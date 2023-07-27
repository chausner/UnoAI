module CardRankingBot

open Card
open Game
open Bot
open Utils

type CardRankingBotSettings =
    { Ranks: int []
      MinCardCounts: int []
      PlayDrawnCardThresholds: int []
      ChooseMostCommonColor: bool }

/// <summary>
/// Bot that chooses cards based on a fixed ranking.
///
/// * Out of all playable cards, the card that has the highest rank is chosen.
/// * When a drawn card can be played, it is always played.
/// * When a Wild or WildDrawFour is played, the color that is most common in the player's hand is chosen.
/// </summary>
type CardRankingBot(game: Game, player: Player, settings: CardRankingBotSettings) =
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
//            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard chooseColor))
            DrawCardBotAction (fun drawnCard ->
                let threshold = settings.PlayDrawnCardThresholds[cardTypeIndex drawnCard]
                if game.Players[player].Length <= threshold || threshold = -1 then
                    Some (chooseColorIfNeeded drawnCard chooseColor)
                else
                    None)

    static member Factory(settings: CardRankingBotSettings) =
        fun game player -> new CardRankingBot(game, player, settings) :> Bot

    static member DefaultSettingsWinRate =
        { Ranks = [| 4; 3; 5; 6; 1; 2 |] // optimal ranking to optimize win rate against 3 other random bots (based on 10,000,000 games)
          MinCardCounts = [| -1; -1; -1; -1; 2; 4 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true }

    static member DefaultSettingsAvgPoints =
        { Ranks = [| 5; 3; 6; 4; 2; 1 |] // optimal ranking to optimize average points against 3 other random bots (based on 10,000,000 games)
          MinCardCounts = [| -1; -1; -1; -1; 5; 3 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true }
