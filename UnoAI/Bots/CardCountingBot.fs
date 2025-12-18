module CardCountingBot

open Card
open Game
open Bot
open Utils
open CardCounting

type CardCountingBotSettings =
    { Weights: float [] }

type private GameView =
    { OwnCards: Card list
      PlayerCardCounts: int []
      ActivePlayer: Player
      Direction: Direction
      DiscardPile: Card list }

type CardCountingBot(game: Game, player: Player, settings: CardCountingBotSettings) =
    inherit Bot()

    let mutable cardCountingContext: CardCountingContext = Array.empty

    let cardScoringFunction card =
        match card with
        | StandardCard (_, _) -> settings.Weights[0]
        | Reverse _           -> settings.Weights[1]
        | Skip _              -> settings.Weights[2]
        | DrawTwo _           -> settings.Weights[3]
        | Wild _              -> settings.Weights[4]
        | WildDrawFour _      -> settings.Weights[5]

    let scoreFunction (view: GameView) =
        // TODO: this breaks for 2 players and Reverse/Skip cards
        // assert (view.ActivePlayer <> player)

        seq {
            // number of own cards
            //yield (view.OwnCards |> List.length |> float, settings.Weights[0])

            // scores of own cards
            yield (view.OwnCards |> Seq.sumBy cardScoringFunction, 1.0)

            // number of opponent's cards
            //yield (((view.PlayerCardCounts |> Seq.sum) - view.OwnCards.Length) |> float, settings.Weights[1])

            // minimum number of cards an opponent has
            //let minCardsOfOpponent = view.PlayerCardCounts |> Seq.indexed |> Seq.filter (fst >> (<>) player) |> Seq.map snd |> Seq.min
            //yield ((minCardsOfOpponent |> float) ** -1.0, settings.Weights[7])

            // number of own cards with same color as discard pile // maybe better as fraction
            //yield (view.OwnCards |> Seq.choose getCardColor |> Seq.filter ((=) (getCardColor view.DiscardPile.Head).Value) |> Seq.length |> float, settings.Weights[3])

            // number of distinct colors in own cards
            //yield (view.OwnCards |> Seq.choose getCardColor |> Seq.distinct |> Seq.length |> float, settings.Weights[4])

            let nextOpponent =
                if view.ActivePlayer <> player then
                    view.ActivePlayer
                else
                    assert (game.NumPlayers = 2)
                    (view.ActivePlayer + 1) %% game.NumPlayers

            // number of cards of next opponent
            //yield (view.PlayerCardCounts[opponent] |> float, settings.Weights[5])

            //let numMatchingCards =
            //    cardCountingContext[opponent]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.sumBy snd
            //yield (float numMatchingCards, settings.Weights[6]) // alone: 15.800%±0.101%  30.7         63.9, with: 22.011%±0.115%  44.5         59.2

            //let numMatchingCards =
            //    cardCountingContext[opponent]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.sumBy snd
            //let matchingCardProb = numMatchingCards * view.PlayerCardCounts[opponent]
            //yield (float matchingCardProb, settings.Weights[6]) // alone: 15.520%±0.224%

            let numMatchingCards =
                cardCountingContext[nextOpponent]
                |> Map.toSeq
                |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
                |> Seq.sumBy snd
            let totalCount = cardCountingContext[nextOpponent] |> Map.values |> Seq.sum
            let matchingCardProb = float (numMatchingCards * view.PlayerCardCounts[nextOpponent]) / float totalCount
            yield (matchingCardProb, settings.Weights[6]) // alone: 15.719%±0.226%, with: 22.144%±0.115%  44.8         59.1

        //let numMatchingCards =
        //    cardCountingContext[opponent]
        //    |> Map.toSeq
        //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
        //    |> Seq.length
        //yield (float numMatchingCards, settings.Weights[6]) // alone: 15.441%±0.224%

        //let matchingCardsScore =
        //    cardCountingContext[opponent]
        //    |> Map.toSeq
        //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
        //    |> Seq.sumBy (fun (card, count) -> count * getCardScore card)
        //yield (float matchingCardsScore, settings.Weights[6]) // alone: 15.174%±0.222%

        //let matchingCardsScore =
        //    cardCountingContext[opponent]
        //    |> Map.toSeq
        //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
        //    |> Seq.sumBy (fun (card, count) -> float count * cardScoringFunction card * -1.0)
        //yield (float matchingCardsScore, settings.Weights[6]) // alone: 15.847%±0.101%  30.9         63.8, with: 22.070%±0.115%  44.6         59.2
        }
        |> Seq.sumBy (fun (value, weight) -> value * weight)

    let getAllColorOptions card =
        match card with
        | Wild None         -> allCardColors |> Seq.map (fun c -> Wild (Some c))
        | WildDrawFour None -> allCardColors |> Seq.map (fun c -> WildDrawFour (Some c))
        | _                 -> Seq.singleton card

    let getView () =
        { OwnCards = game.Players[player]
          PlayerCardCounts = game.Players |> Array.map List.length
          ActivePlayer = game.ActivePlayer
          Direction = game.Direction
          DiscardPile = game.DiscardPile }

    let applyPlayCardAction card view =
        let removeCardFromHand card hand =
            let i = hand |> List.findIndex ((=) (removeColor card))
            hand |> List.removeAt i

        let ownCards' = removeCardFromHand card view.OwnCards

        let direction' =
            match card with
            | Reverse _ when game.RuleSet.TwoPlayerReverseIsSkip && game.NumPlayers = 2 -> view.Direction
            | Reverse _ ->
                match view.Direction with
                | Clockwise        -> Counterclockwise
                | Counterclockwise -> Clockwise
            | _ -> view.Direction

        let discardPile' = card :: view.DiscardPile

        let advance steps : Player =
            let delta =
                match direction' with
                | Clockwise -> steps
                | Counterclockwise -> -steps

            (view.ActivePlayer + delta) %% game.NumPlayers

        let activePlayer' =
            match card with
            | StandardCard (_, _) -> advance 1
            | Skip _              -> advance 2
            | DrawTwo _           -> advance 2
            | Reverse _ when game.RuleSet.TwoPlayerReverseIsSkip && game.NumPlayers = 2 -> advance 2
            | Reverse _           -> advance 1
            | Wild _              -> advance 1
            | WildDrawFour _      -> advance 2

        let playerCardCounts' = Array.copy view.PlayerCardCounts

        playerCardCounts'[player] <- playerCardCounts'[player] - 1

        match card with
        | DrawTwo _      -> playerCardCounts'[advance 1] <- playerCardCounts'[advance 1] + 2
        | WildDrawFour _ -> playerCardCounts'[advance 1] <- playerCardCounts'[advance 1] + 4
        | _              -> ()

        { OwnCards = ownCards'
          PlayerCardCounts = playerCardCounts'
          ActivePlayer = activePlayer'
          Direction = direction'
          DiscardPile = discardPile' }

    let applyDrawCardAction (view: GameView) =
        let advance steps : Player =
            let delta =
                match view.Direction with
                | Clockwise        -> steps
                | Counterclockwise -> -steps

            (view.ActivePlayer + delta) %% game.NumPlayers

        let playerCardCounts' = Array.copy view.PlayerCardCounts

        playerCardCounts'[player] <- playerCardCounts'[player] + 1

        let cardDrawn = fullCardDeck |> List.chooseRandom

        { OwnCards = cardDrawn :: view.OwnCards
          PlayerCardCounts = playerCardCounts'
          ActivePlayer = advance 1
          Direction = view.Direction
          DiscardPile = view.DiscardPile }

    let applyAdvance view =
        let nextPlayer: Player =
            match view.Direction with
            | Clockwise        -> (view.ActivePlayer + 1) %% game.NumPlayers
            | Counterclockwise -> (view.ActivePlayer - 1) %% game.NumPlayers

        { view with ActivePlayer = nextPlayer }

    let playDrawnCardCallback drawnCard =
        let view = getView ()

        let options =
            seq {
                yield!
                    getAllColorOptions drawnCard
                    |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, view |> applyAdvance |> scoreFunction
            }

        options |> Seq.maxBy snd |> fst

    let sanityCheck (ccc: CardCountingContext) action =
        match action with
        | Some (PlayCardAction (_, (DrawTwo _)))
        | Some (PlayCardAction (_, (WildDrawFour _)))
        | Some (DrawAndPlayCardAction (_, (DrawTwo _)))
        | Some (DrawAndPlayCardAction (_, (WildDrawFour _))) -> ()
        | _ ->
            [ 0 .. game.NumPlayers - 1 ]
            |> Seq.filter ((<>) player)
            |> Seq.iter (fun p ->
                game.Players[p]
                |> Seq.countBy id
                |> Seq.filter (fun (card, count) -> ccc[p][card] < count)
                |> Seq.iter (fun (card, count) ->
                    printfn "%A: cardCountingContext: %i, really: %i (%A)" card (ccc[p][card]) count action))
            ()

    override self.Initialize state =
        cardCountingContext <- initCardCountingContext state player
        //sanityCheck cardCountingContext None

    override self.OnActionPerformed prevState action state =
        //let newccc = updateCardCountingContext cardCountingContext prevState action state player
        //sanityCheck newccc (Some action)
        cardCountingContext <- updateCardCountingContext cardCountingContext prevState action state player

    override self.PerformAction() =
        let playableCards =
            game.Players[player]
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard

        let view = getView ()

        let options =
            seq {
                yield!
                    playableCards
                    |> Seq.collect getAllColorOptions
                    |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                //yield None, view |> applyDrawCardAction |> scoreFunction
                yield None, -999999.0
            }

        let bestOption = options |> Seq.maxBy snd |> fst

        match bestOption with
        | Some card -> PlayCardBotAction card
        | None      -> DrawCardBotAction playDrawnCardCallback

    static member Factory(settings: CardCountingBotSettings) =
        fun game player -> new CardCountingBot(game, player, settings) :> Bot

    static member DefaultSettingsWinRate =
        { Weights = [| -0.168; -0.163; -0.190; -0.227; 0.048; -0.035; -0.0001; 0.0 |] } // weights optimized on win rate against 3 other random bots

    static member DefaultSettingsAvgPoints =
        { Weights = [| -0.189; -0.153; -0.206; -0.193; 0.015; 0.023; -0.0001; 0.0 |] } // weights optimized on average points against 3 other random bots
