module ScoreBot

open Card
open Game
open Bot
open Utils
open CardCounting

type GameView = {
    OwnCards : Card list;
    PlayerCardCounts: int [];
    ActivePlayer : Player; 
    Direction : Direction;
    DiscardPile : Card list }

let allCardsExceptWilds =
    fullCardDeck
    |> Seq.distinct
    |> Seq.filter (fun card -> card <> Wild None && card <> WildDrawFour None)
    |> Seq.toArray

type ScoreBot(game : Game, player : Player, weights : float []) =
    inherit Bot()

    let mutable cardCountingContext : CardCountingContext = Array.empty

    let weightsWinRate   = [| -0.90; -0.64; -0.62; -0.81; 0.55; 0.48; 0.14; 0.13 |]
    let weightsAvgPoints = [| -0.81; -0.45; -0.85; -0.52; 0.14; 0.70; 0.08; 0.06 |]

    let cardScoringFunction (weights : float []) card =
        match card with                     
        | StandardCard (n, _) -> weights.[0]
        | Reverse _           -> weights.[1]
        | Skip _              -> weights.[2]
        | DrawTwo _           -> weights.[3]
        | Wild _              -> weights.[4]
        | WildDrawFour _      -> weights.[5]

    let scoreFunction (view : GameView) =  
        view.OwnCards |> Seq.sumBy (cardScoringFunction weightsWinRate)
                                          
//    let scoreFunction (view : GameView) =  
//        seq {
//            // number of own cards
//            yield (view.OwnCards |> List.length |> float, weights.[0])
//
//            // scores of own cards
//            //yield (view.OwnCards |> Seq.sumBy cardScoringFunction, 1.0)
//
//            // number of opponent's cards
//            yield (((view.PlayerCardCounts |> Seq.sum) - view.OwnCards.Length) |> float, weights.[1]) 
//
//            // minimum number of cards an opponent has
//            yield (view.PlayerCardCounts |> Seq.indexed |> Seq.filter (fst >> (<>) player) |> Seq.map snd |> Seq.min |> float, weights.[2])
//
//            // number of own cards with same color as discard pile // maybe better as fraction
//            yield (view.OwnCards |> Seq.choose getCardColor |> Seq.filter ((=) (getCardColor view.DiscardPile.Head).Value) |> Seq.length |> float, weights.[3]) 
//
//            // number of distinct colors in own cards
//            yield (view.OwnCards |> Seq.choose getCardColor |> Seq.distinct |> Seq.length |> float, weights.[4])
//
//            // number of cards of active player (not good when ActivePlayer = player though)
//            //yield (view.PlayerCardCounts.[view.ActivePlayer] |> float, weights.[5])
//
////            match view.DiscardPile.Head with
////            | Wild _ | WildDrawFour _ -> ()
////            | Skip _ | DrawTwo _ | Reverse _ -> ()
////            | _ ->
////            let hasNoMatchingCard =
////                allCardsExceptWilds
////                |> Seq.filter (doCardsMatch view.DiscardPile.Head)
////                |> Seq.forall (fun card -> cardCountingContext.[view.ActivePlayer].[card] = 0)
////
////            if hasNoMatchingCard then
//////                printfn "Has not: %A" view.DiscardPile.Head
//////                game.Players.[view.ActivePlayer] |> Seq.iter (printfn "%A")
//////                printfn ""
//////                if game.Players.[view.ActivePlayer] 
//////                    |> Seq.filter (fun card -> card <> Wild None && card <> WildDrawFour None)
//////                    |> Seq.exists (doCardsMatch view.DiscardPile.Head) then
//////                    printfn "WTF %i %i %A" view.ActivePlayer player view.DiscardPile.Head
////                    yield (1.0, weights.[9])
//        }
//        |> Seq.sumBy (fun (value, weight) -> value * weight)

    let getAllColorOptions card =
        let colors = [ Red; Green; Blue; Yellow ]
        match card with
        | Wild None         -> colors |> Seq.map (fun c -> Wild (Some c))
        | WildDrawFour None -> colors |> Seq.map (fun c -> WildDrawFour (Some c))
        | _                 -> Seq.singleton card

    let getView() = {
        OwnCards = game.Players.[player];
        PlayerCardCounts = game.Players |> Array.map List.length;
        ActivePlayer = game.ActivePlayer;
        Direction = game.Direction;
        DiscardPile = game.DiscardPile }

    let applyPlayCardAction card view =
        let rec removeCardFromHand card hand =
            match hand with
            | []      -> []
            | x :: xs -> if isCardInHand card x then xs else x :: removeCardFromHand card xs

        let ownCards' = removeCardFromHand card view.OwnCards

        let direction' =
            match card, view.Direction with
            | Reverse _, Clockwise        -> Counterclockwise
            | Reverse _, Counterclockwise -> Clockwise
            | _,         _                -> view.Direction

        let discardPile' = card :: view.DiscardPile

        let advance steps : Player =
            let delta =
                match direction' with
                | Clockwise        -> steps
                | Counterclockwise -> -steps
            (view.ActivePlayer + steps) %% view.PlayerCardCounts.Length

        let activePlayer' = 
            match card with
            | StandardCard (_, _) -> advance 1
            | Skip _              -> advance 2
            | DrawTwo _           -> advance 2
            | Reverse _           -> advance 1
            | Wild _              -> advance 1
            | WildDrawFour _      -> advance 2

        let playerCardCounts' = Array.copy view.PlayerCardCounts
        
        playerCardCounts'.[player] <- playerCardCounts'.[player] - 1

        match card with
        | DrawTwo _      -> playerCardCounts'.[advance 1] <- playerCardCounts'.[advance 1] + 2
        | WildDrawFour _ -> playerCardCounts'.[advance 1] <- playerCardCounts'.[advance 1] + 4
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
            (view.ActivePlayer + steps) %% view.PlayerCardCounts.Length

        let playerCardCounts' = Array.copy view.PlayerCardCounts
        
        playerCardCounts'.[player] <- playerCardCounts'.[player] + 1

        { OwnCards = StandardCard (5, Red) :: view.OwnCards;
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
        let options =
            seq {
                let view = getView()

                yield! getAllColorOptions drawnCard
                       |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, view |> applyAdvance |> scoreFunction }

        options |> Seq.maxBy snd |> fst
    
    let sanityCheck (ccc : CardCountingContext) act =
        match act with
        | Some (PlayCardAction (_, (DrawTwo _)))
        | Some (PlayCardAction (_, (WildDrawFour _)))
        | Some (DrawAndPlayCardAction (_, (DrawTwo _)))
        | Some (DrawAndPlayCardAction (_, (WildDrawFour _))) -> ()
        | _ ->

        [0..game.Players.Length-1]
        |> Seq.filter ((<>) player)
        |> Seq.iter (fun p ->
            game.Players.[p]
            |> Seq.countBy id
            |> Seq.filter (fun (card, count) -> ccc.[p].[card] < count)
            |> Seq.iter (fun (card, count) ->
                printfn "%A: cardCountingContext: %i, really: %i (%A)" card ccc.[p].[card] count act))
        ()

//    override self.Initialize state =
//        cardCountingContext <- getInitialCardCountingContext state player
//        //sanityCheck cardCountingContext None
//
//    override self.OnActionPerformed prevState action state =
//        //let newccc = getCardCountingContext cardCountingContext action state player
//        //sanityCheck newccc (Some action)
//        cardCountingContext <- getCardCountingContext cardCountingContext prevState action state player        

    override self.PerformAction() =
        let playableCards =            
            game.Players.[player]  
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard

        let view = getView()

        let options = 
            seq {
                yield! playableCards
                       |> Seq.collect getAllColorOptions
                       |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, view |> applyDrawCardAction |> scoreFunction
            }

        let bestOption = options |> Seq.maxBy snd |> fst

        match bestOption with
        | Some card -> PlayCardBotAction card
        | None      -> DrawCardBotAction playDrawnCardCallback