module CardCountingBot

open Card
open Game
open Bot
open Utils
open CardCounting
open BotRunner
open RandomBot
open System
open System.Globalization
open System.IO

type private GameView = {
    OwnCards : Card list;
    PlayerCardCounts: int [];
    ActivePlayer : Player; 
    Direction : Direction;
    DiscardPile : Card list }

type CardCountingBot(game : Game, player : Player, weights : float []) =
    inherit Bot()

    let mutable cardCountingContext : CardCountingContext = Array.empty

    let cardScoringFunction card =
        match card with                     
        | StandardCard (_, _) -> weights[0]
        | Reverse _           -> weights[1]
        | Skip _              -> weights[2]
        | DrawTwo _           -> weights[3]
        | Wild _              -> weights[4]
        | WildDrawFour _      -> weights[5]
                                          
    let scoreFunction (view : GameView) =  
        assert (view.ActivePlayer <> player)

        seq {
            // number of own cards
            //yield (view.OwnCards |> List.length |> float, weights[0])

            // scores of own cards
            yield (view.OwnCards |> Seq.sumBy cardScoringFunction, 1.0)

            // number of opponent's cards
            //yield (((view.PlayerCardCounts |> Seq.sum) - view.OwnCards.Length) |> float, weights[1]) 

            // minimum number of cards an opponent has
            //let minCardsOfOpponent = view.PlayerCardCounts |> Seq.indexed |> Seq.filter (fst >> (<>) player) |> Seq.map snd |> Seq.min
            //yield ((minCardsOfOpponent |> float) ** -1.0, weights[7])

            // number of own cards with same color as discard pile // maybe better as fraction
            //yield (view.OwnCards |> Seq.choose getCardColor |> Seq.filter ((=) (getCardColor view.DiscardPile.Head).Value) |> Seq.length |> float, weights[3]) 

            // number of distinct colors in own cards
            //yield (view.OwnCards |> Seq.choose getCardColor |> Seq.distinct |> Seq.length |> float, weights[4])

            // number of cards of active player
            //yield (view.PlayerCardCounts[view.ActivePlayer] |> float, weights[5])

            //let numMatchingCards =
            //    cardCountingContext[view.ActivePlayer]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.sumBy snd
            //yield (float numMatchingCards, weights[6]) // alone: 15.800%±0.101%  30.7         63.9, with: 22.011%±0.115%  44.5         59.2

            //let numMatchingCards =
            //    cardCountingContext[view.ActivePlayer]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.sumBy snd
            //let matchingCardProb = numMatchingCards * view.PlayerCardCounts[view.ActivePlayer]
            //yield (float matchingCardProb, weights[6]) // alone: 15.520%±0.224%
            
            let numMatchingCards =
                cardCountingContext[view.ActivePlayer]
                |> Map.toSeq
                |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
                |> Seq.sumBy snd
            let totalCount = cardCountingContext[view.ActivePlayer] |> Map.values |> Seq.sum
            let matchingCardProb = float (numMatchingCards * view.PlayerCardCounts[view.ActivePlayer]) / float totalCount
            yield (matchingCardProb, weights[6]) // alone: 15.719%±0.226%, with: 22.144%±0.115%  44.8         59.1

            //let numMatchingCards =
            //    cardCountingContext[view.ActivePlayer]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.length
            //yield (float numMatchingCards, weights[6]) // alone: 15.441%±0.224%

            //let matchingCardsScore =
            //    cardCountingContext[view.ActivePlayer]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.sumBy (fun (card, count) -> count * getCardScore card)
            //yield (float matchingCardsScore, weights[6]) // alone: 15.174%±0.222%

            //let matchingCardsScore =
            //    cardCountingContext[view.ActivePlayer]
            //    |> Map.toSeq
            //    |> Seq.filter (fun (card, count) -> doCardsMatch view.DiscardPile.Head card)
            //    |> Seq.sumBy (fun (card, count) -> float count * cardScoringFunction card * -1.0)
            //yield (float matchingCardsScore, weights[6]) // alone: 15.847%±0.101%  30.9         63.8, with: 22.070%±0.115%  44.6         59.2
        }
        |> Seq.sumBy (fun (value, weight) -> value * weight)

    let getAllColorOptions card =
        let colors = [| Red; Green; Blue; Yellow |]
        match card with
        | Wild None         -> colors |> Seq.map (fun c -> Wild (Some c))
        | WildDrawFour None -> colors |> Seq.map (fun c -> WildDrawFour (Some c))
        | _                 -> Seq.singleton card

    let getView() = {
        OwnCards = game.Players[player];
        PlayerCardCounts = game.Players |> Array.map List.length;
        ActivePlayer = game.ActivePlayer;
        Direction = game.Direction;
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
                | Clockwise        -> steps
                | Counterclockwise -> -steps
            (view.ActivePlayer + delta) %% view.PlayerCardCounts.Length

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

        { OwnCards = ownCards';
          PlayerCardCounts = playerCardCounts';
          ActivePlayer = activePlayer';
          Direction = direction';
          DiscardPile = discardPile' }

    let applyDrawCardAction (view : GameView) = 
        let advance steps : Player =
            let delta =
                match view.Direction with
                | Clockwise        -> steps
                | Counterclockwise -> -steps
            (view.ActivePlayer + delta) %% view.PlayerCardCounts.Length

        let playerCardCounts' = Array.copy view.PlayerCardCounts
        
        playerCardCounts'[player] <- playerCardCounts'[player] + 1

        let cardDrawn = fullCardDeck |> List.chooseRandom

        { OwnCards = cardDrawn :: view.OwnCards;
          PlayerCardCounts = playerCardCounts';
          ActivePlayer = advance 1;
          Direction = view.Direction;
          DiscardPile = view.DiscardPile }

    let applyAdvance view =
        let nextPlayer : Player =
            match view.Direction with
            | Clockwise        -> (view.ActivePlayer + 1) %% view.PlayerCardCounts.Length
            | Counterclockwise -> (view.ActivePlayer - 1) %% view.PlayerCardCounts.Length
        { view with ActivePlayer = nextPlayer }

    let playDrawnCardCallback drawnCard =
        let view = getView()

        let options =
            seq {
                yield! getAllColorOptions drawnCard
                       |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, view |> applyAdvance |> scoreFunction }

        options |> Seq.maxBy snd |> fst
    
    let sanityCheck (ccc : CardCountingContext) action =
        match action with
        | Some (PlayCardAction (_, (DrawTwo _)))
        | Some (PlayCardAction (_, (WildDrawFour _)))
        | Some (DrawAndPlayCardAction (_, (DrawTwo _)))
        | Some (DrawAndPlayCardAction (_, (WildDrawFour _))) -> ()
        | _ ->

        [0..game.NumPlayers-1]
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

        let view = getView()

        let options = 
            seq {
                yield! playableCards
                       |> Seq.collect getAllColorOptions
                       |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                //yield None, view |> applyDrawCardAction |> scoreFunction
                yield None, -999999.0
            }

        let bestOption = options |> Seq.maxBy snd |> fst

        match bestOption with
        | Some card -> PlayCardBotAction card
        | None      -> DrawCardBotAction playDrawnCardCallback

    static member Factory(weights : float []) =
        fun (game, player) -> new CardCountingBot(game, player, weights) :> Bot

    static member WeightsWinRate = [| -0.168; -0.163; -0.190; -0.227; 0.048; -0.035; -0.0001; 0.0 |] // weights optimized on win rate against 3 other random bots

let optimizeWeights() =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 100_000

    let printScoring (scoring : float []) =
        String.concat " " (scoring |> Seq.map (fun x -> x.ToString("F3", CultureInfo.InvariantCulture).PadLeft(6)))

    let ranges = [ (-0.26, -0.17); (-0.26, -0.17); (-0.22, -0.08); (-0.32, -0.10); (-0.35, -0.20); (-0.13, 0.14); (-0.12, 0.08) ]

    let getRandomInRanges ranges =
        ranges
        |> Seq.map (fun (l, h) -> Random.Shared.NextDouble() * (h - l) + l)
        |> Seq.toArray

    let getRandomNear scoring margin =
        scoring |> Array.map (fun x -> x * (1.0 + Random.Shared.NextDouble() * 2.0 * margin - margin))

    let normalizeScoring scoring =
        let s = scoring |> Seq.map abs |> Seq.sum
        scoring |> Array.map (fun x -> x / s)

    let pid = System.Environment.ProcessId
    use output = File.CreateText(@$"C:\Users\chris\Desktop\UnoAI\run5\{pid}.csv")
    output.AutoFlush <- true

    //for scoring in Seq.initInfinite (fun _ -> Array.init 6 (fun _ -> Random.Shared.NextDouble() * 2.0 - 1.0)) do
    //for scoring in Seq.initInfinite (fun _ -> getRandomInRanges ranges) |> Seq.map normalizeScoring do
    //for scoring in Seq.initInfinite (fun _ -> getRandomNear [| -0.359;0.431;0.041;0.062;0.108 |] 0.2) |> Seq.map normalizeScoring do
    //for scoring in [0.00001..0.01..0.2] |> Seq.map (fun w -> Array.append scoring' [| w |]) do
    for param in [1..40] |> Seq.map (fun p -> -0.0001 * float p) do
        //let scoring = scoring |> Seq.toArray
        let scoring = CardCountingBot.WeightsWinRate
        scoring[6] <- param

        let bots =     
            CardCountingBot.Factory(scoring) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray
            
        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printScoring scoring) winRate averagePoints
            output.WriteLine(String.Join(';', (Array.append scoring [| winRate; averagePoints |])))
            
        with
        | e -> printfn "error"