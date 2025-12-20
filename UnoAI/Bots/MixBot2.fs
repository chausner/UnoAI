module MixBot2

open Card
open Game
open Bot
open Utils
open CardCounting
open BotUtils

type MixBot2Settings =
    { Ranks: int []
      EndGameRanks: int []
      PrioritizeMostCommonColor: bool
      CardCountLimits: int []
      EndGameCardCountLimits: int []
      PlayDrawnCardThresholds: int []
      ChooseMostCommonColor: bool
      EnableEndGameLogic: bool
      UseCardCounting: bool
      WildPreferredColorSlack: int }

type MixBot2(game: Game, player: Player, settings: MixBot2Settings) =
    inherit Bot()

    let mutable cardCountingContext: CardCountingContext = Array.empty

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

    let advanceFrom (p: Player) (direction: Direction) steps : Player =
        let delta =
            match direction with
            | Clockwise -> steps
            | Counterclockwise -> -steps
        (p + delta) %% game.NumPlayers

    let getNextPlayer (p: Player) (direction: Direction) =
        advanceFrom p direction 1

    let getNextActiveAfterPlay (activePlayer: Player) (direction: Direction) card =
        match card with
        | StandardCard _ -> direction, advanceFrom activePlayer direction 1
        | Skip _ -> direction, advanceFrom activePlayer direction 2
        | DrawTwo _ -> direction, advanceFrom activePlayer direction 2
        | Wild _ -> direction, advanceFrom activePlayer direction 1
        | WildDrawFour _ -> direction, advanceFrom activePlayer direction 2
        | Reverse _ when game.RuleSet.TwoPlayerReverseIsSkip && game.NumPlayers = 2 ->
            direction, advanceFrom activePlayer direction 2
        | Reverse _ ->
            let direction' =
                match direction with
                | Clockwise -> Counterclockwise
                | Counterclockwise -> Clockwise
            direction', advanceFrom activePlayer direction' 1

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

    let removeOneFromHand card (hand: Card list) =
        let i = hand |> List.findIndex ((=) (removeColor card))
        hand |> List.removeAt i

    let probabilityNoMatchByCardCounting (p: Player) (topCard: Card) =
        let counts = cardCountingContext[p]
        if counts.IsEmpty then
            0.0
        else
            let totalCount = counts |> Map.values |> Seq.sum
            let matchingCount =
                counts
                |> Map.toSeq
                |> Seq.sumBy (fun (card, count) -> if doCardsMatch topCard card then count else 0)

            let handSize = game.Players[p].Length
            let draws = min handSize totalCount
            if totalCount = 0 then
                0.0
            elif matchingCount = 0 then
                1.0
            elif totalCount - matchingCount <= draws - 1 then
                0.0
            else
                seq { 0..draws - 1 }
                |> Seq.fold (fun probability i ->
                    probability * float (totalCount - matchingCount - i) / float (totalCount - i)
                ) 1.0

    let expandWildOptions card =
        match card with
        | Wild None -> allCardColors |> Seq.map (fun c -> Wild (Some c))
        | WildDrawFour None -> allCardColors |> Seq.map (fun c -> WildDrawFour (Some c))
        | _ -> Seq.singleton card

    let isEndGame () =
        if not settings.EnableEndGameLogic then
            false
        else
            let nextPlayer = getNextPlayer game.ActivePlayer game.Direction
            game.Players[nextPlayer].Length = 1

    override self.Initialize state =
        cardCountingContext <- initCardCountingContext state player

    override self.OnActionPerformed prevState action state =
        cardCountingContext <- updateCardCountingContext cardCountingContext prevState action state player

    override self.PerformAction() =
        let cardCountLimits =
            if isEndGame () then
                settings.EndGameCardCountLimits
            else
                settings.CardCountLimits

        let numCardsInHand =
            game.Players[player]
            |> Seq.filter (fun card -> cardCountLimits[cardTypeIndex card] = -1)
            |> Seq.length

        let playableCards =
            game.Players[player]
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard

        let playableCardsWithinLimits =
            playableCards
            |> Seq.filter (fun card ->
                let limit = cardCountLimits[cardTypeIndex card]
                numCardsInHand <= limit || limit = -1)
            |> Seq.toList

        let chooseCard (cards: Card list) =
            let mostCommonColors = getMostCommonColors ()
            let preferredWildColors =
                if not settings.PrioritizeMostCommonColor then
                    allCardColors
                else
                    let colorCounts =
                        game.Players[player]
                        |> Seq.choose getCardColor
                        |> Seq.countBy id
                        |> Map.ofSeq

                    if colorCounts.IsEmpty then
                        allCardColors
                    else
                        let maxCount = colorCounts |> Seq.map (fun (KeyValue (_, v)) -> v) |> Seq.max
                        colorCounts
                        |> Seq.filter (fun (KeyValue (_, v)) -> v >= maxCount - settings.WildPreferredColorSlack)
                        |> Seq.map (fun (KeyValue (c, _)) -> c)
                        |> Seq.toArray

            let baseRank card =
                if isEndGame () then
                    endGameScoringFunction card
                else
                    scoringFunction card

            let colorPriority card =
                match card with
                | Wild (Some color)
                | WildDrawFour (Some color) ->
                    if preferredWildColors |> Array.contains color then 1 else 0
                | _ ->
                    match getCardColor card with
                    | Some color when settings.PrioritizeMostCommonColor && (mostCommonColors |> List.contains color) -> 1
                    | _ -> 0

            let cardCountingScore card =
                let _, nextActive = getNextActiveAfterPlay game.ActivePlayer game.Direction card
                if nextActive = player then
                    let handAfter = game.Players[player] |> removeOneFromHand card
                    let ownPlayable = handAfter |> Seq.exists (doCardsMatch card)
                    if ownPlayable then 0.0 else 1.0
                else
                    probabilityNoMatchByCardCounting nextActive card

            cards
            |> Seq.collect expandWildOptions
            |> Seq.pickMaxBy baseRank
            |> Seq.pickMaxBy colorPriority
            |> (if settings.UseCardCounting then Seq.pickMaxBy cardCountingScore else id)
            |> List.chooseRandom

        if not (playableCardsWithinLimits |> List.isEmpty) then
            let playedCard = chooseCard playableCardsWithinLimits
            PlayCardBotAction playedCard
        else
            DrawCardBotAction (fun drawnCard ->
                let threshold = settings.PlayDrawnCardThresholds[cardTypeIndex drawnCard]
                if game.Players[player].Length <= threshold || threshold = -1 then
                    Some (chooseColorIfNeeded drawnCard chooseColor)
                else
                    None)

    static member Factory(settings: MixBot2Settings) =
        fun game player -> new MixBot2(game, player, settings) :> Bot

    static member DefaultSettingsWinRate =
        { Ranks = [| 4; 3; 5; 6; 1; 2 |]
          EndGameRanks = [| 2; 3; 4; 6; 1; 5 |]
          PrioritizeMostCommonColor = true
          CardCountLimits = [| -1; -1; -1; -1; 2; 4 |]
          EndGameCardCountLimits = [| -1; -1; -1; -1; -1; -1 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true
          EnableEndGameLogic = true
          UseCardCounting = true
          WildPreferredColorSlack = 1 }

    static member DefaultSettingsAvgPoints =
        { Ranks = [| 5; 3; 6; 4; 2; 1 |]
          EndGameRanks = [| 2; 3; 4; 6; 1; 5 |]
          PrioritizeMostCommonColor = true
          CardCountLimits = [| -1; -1; -1; -1; 5; 3 |]
          EndGameCardCountLimits = [| -1; -1; -1; -1; -1; -1 |]
          PlayDrawnCardThresholds = [| -1; -1; -1; -1; 2; 2 |]
          ChooseMostCommonColor = true
          EnableEndGameLogic = true
          UseCardCounting = true
          WildPreferredColorSlack = 2 }