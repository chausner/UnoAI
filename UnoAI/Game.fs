module Game

open Utils
open Card

type Player = int

type Direction =
    | Clockwise
    | Counterclockwise

type Status =
    | Playing
    | Ended of Player * int

type State = {
    ActivePlayer : Player; 
    Direction : Direction;
    Status : Status;
    Players : Card list [];
    DiscardPile : Card list;
    DrawPile : Card list }

type Action =
    | PlayCardAction of Player * Card
    | DrawCardAction of Player * Card
    | DrawAndPlayCardAction of Player * Card
    | DrawCardsAndSkipAction of Player * Card list

type Game(numPlayers, dealer : Player) =
    do if numPlayers < 2 || numPlayers > 10 then
        invalidArg "numPlayers" "The number of players must be between 2 and 10."
    do if dealer < 0 || dealer >= numPlayers then
        invalidArg "dealer" "Invalid player."

    let mutable activePlayer : Player = (dealer + 1) %% numPlayers

    let mutable direction = Clockwise   
   
    let mutable status = Playing

    let reverseDirection() =
        direction <-
            match direction with
            | Clockwise -> Counterclockwise
            | Counterclockwise -> Clockwise

    let advance steps =
        let delta =
            match direction with
            | Clockwise        -> steps
            | Counterclockwise -> -steps
            
        activePlayer <- (activePlayer + delta) %% numPlayers

    let playerCards, remainingCards =
        fullCardDeck
        |> List.shuffle
        |> List.splitAt (numPlayers * 7)

    let players =
        playerCards
        |> List.chunkBySize 7
        |> List.toArray

    let mutable discardPile, drawPile =
        let rec getDiscardAndDrawPile cards =
            match cards with
            | StandardCard (_, _) as topCard :: drawPile -> [topCard], drawPile
            | Skip _              as topCard :: drawPile -> advance 1
                                                            [topCard], drawPile
            | DrawTwo _           as topCard :: drawPile -> //drawCards activePlayer 2
                                                            //advance 1
                                                            //[topCard], drawPile
                                                            players.[activePlayer] <- (players.[activePlayer] |> List.take 2 |> List.rev) @ players.[activePlayer]
                                                            advance 1
                                                            [topCard], drawPile |> List.skip 2
            | Reverse _           as topCard :: drawPile -> reverseDirection()
                                                            activePlayer <- dealer
                                                            [topCard], drawPile
            | Wild _              as topCard :: drawPile -> [topCard], drawPile
            | WildDrawFour _      as topCard :: drawPile -> getDiscardAndDrawPile (cards |> List.shuffle)
    
        getDiscardAndDrawPile remainingCards

    let refillDrawPile() =
        if not (drawPile |> List.isEmpty) then
            invalidOp "Draw pile can only be refilled when it is empty."
            
        drawPile <- discardPile |> List.tail |> Seq.map removeColor |> List.shuffle
        discardPile <- [discardPile |> List.head]

    let drawCards (player : Player) numCards =
        for i = 1 to numCards do
            let cardDrawn = drawPile |> List.head
            drawPile <- drawPile |> List.tail
            players.[player] <- cardDrawn :: players.[player]
            if drawPile |> List.isEmpty then
                refillDrawPile()   

    let getNextPlayer() : Player =
        let delta =
            match direction with
            | Clockwise        -> 1
            | Counterclockwise -> -1

        (activePlayer + delta) %% numPlayers 

    let endGame winner =
        let score = 
            players
            |> Seq.concat
            |> Seq.sumBy getCardScore
        status <- Ended (winner, score)

    member self.Players = players      

    member self.Direction = direction

    member self.ActivePlayer = activePlayer

    member self.DiscardPile = discardPile

    member self.Status = status

    member self.State =
        { ActivePlayer = activePlayer;
          Direction = direction;
          Status = status;
          Players = players;
          DiscardPile = discardPile;
          DrawPile = drawPile }

    member self.CanPlayCard card =
        if status <> Playing then
            invalidOp "An action can only be performed when the game has not ended yet."

//        if card = Wild None || card = WildDrawFour None then
//            invalidArg "card" "Invalid card."

        let player = self.ActivePlayer

        if not (players.[player] |> List.exists (isCardInHand card)) then
            false
        else
            let topCard = discardPile |> List.head

            // special case when first card in the discard pile is a Wild card
            if topCard = Wild None then
                let topCard' = Wild (getCardColor card)
                discardPile <- topCard' :: (discardPile |> List.skip 1)
                doCardsMatch topCard' card
            else
                doCardsMatch topCard card
            
    member self.PlayCard card =
        if status <> Playing then
            invalidOp "An action can only be performed when the game has not ended yet."

        if not (self.CanPlayCard card) then
            invalidArg "card" "The specified card cannot be played."

        if card = Wild None || card = WildDrawFour None then
            invalidArg "card" "Invalid card."

        let player = self.ActivePlayer

        discardPile <- card :: discardPile

        let rec removeCardFromHand card hand =
            match hand with
            | []      -> []
            | x :: xs -> if isCardInHand card x then xs else x :: removeCardFromHand card xs

        players.[player] <- removeCardFromHand card players.[player]
        
        match card with
        | StandardCard (_, _) -> advance 1
        | Skip _              -> advance 2
        | DrawTwo _           -> drawCards (getNextPlayer()) 2
                                 advance 2
        | Reverse _           -> // some rules say that in case of 2 players, Reverse acts like Skip
                                 reverseDirection()
                                 advance 1
        | Wild _              -> advance 1
        | WildDrawFour _      -> drawCards (getNextPlayer()) 4
                                 advance 2

        if players.[player] |> List.isEmpty then
            endGame player

    member self.DrawCard (playDrawnCardCallback : Card -> Card option) =
        if status <> Playing then
            invalidOp "An action can only be performed when the game has not ended yet."

        let player = self.ActivePlayer

        drawCards player 1 

        let cardDrawn = players.[player] |> List.head

        if self.CanPlayCard cardDrawn then        
            match playDrawnCardCallback cardDrawn with
            | Some c -> if removeColor c <> cardDrawn then
                            failwith "No other card may be played except the one drawn."

                        self.PlayCard c
            | None   -> advance 1
        else
            advance 1