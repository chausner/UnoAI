module CardCounting

open Card
open Game
open Utils

let private maxCardCountsFromFullDeck =
    fullCardDeck
    |> Seq.countBy id
    |> Map.ofSeq

let private allCards = fullCardDeck |> List.distinct

type CardCountingContext = Map<Card, int> []

let private subtractCounts (counts1 : Map<Card, int>) (counts2 : Map<Card, int>) =
    counts1
    |> Map.toSeq
    |> Seq.map (fun (card, count) -> card, count - (counts2 |> Map.tryFind card |? 0))
    |> Map.ofSeq

let private getCountsFromOwnCardsDiscardPile (state : State) (viewpoint : Player) =
    Seq.append state.Players.[viewpoint] state.DiscardPile
    |> Seq.countBy id
    |> Map.ofSeq

let private getInitialCardCounts (initialState : State) viewpoint =    
    subtractCounts maxCardCountsFromFullDeck (getCountsFromOwnCardsDiscardPile initialState viewpoint)

let getInitialCardCountingContext (initialState : State) viewpoint =
    let context = Array.create initialState.Players.Length (getInitialCardCounts initialState viewpoint)
    context.[viewpoint] <- Map.empty
    context

let private getCardCounts (prevContext : CardCountingContext) (prevState : State) action (state : State) viewpoint (player : Player) =
    let prevCardCounts = prevContext.[player]
    let countsFromOwnCardsDiscardPile = getCountsFromOwnCardsDiscardPile state viewpoint
    let discardPileTop = state.DiscardPile.Head
    let rec ensureCardCountIsNotAboveUpperBound (cardCounts : Map<Card, int>) cards =
        match cards with
        | []           -> cardCounts
        | card :: tail ->
            let upperBound = maxCardCountsFromFullDeck.[card] - (countsFromOwnCardsDiscardPile |> Map.tryFind card |? 0)
            let x = 
                if cardCounts.[card] > upperBound then
                    cardCounts |> Map.add card upperBound
                else
                    cardCounts
            ensureCardCountIsNotAboveUpperBound x tail
    match action with
    | PlayCardAction (p, card) when p = player ->
        prevCardCounts
        |> Map.add (removeColor card) (max (prevCardCounts.[removeColor card] - 1) 0)
    | PlayCardAction (p, card) when p = viewpoint -> 
        prevCardCounts
    | PlayCardAction (p, card) -> 
        ensureCardCountIsNotAboveUpperBound prevCardCounts [removeColor card]

    | DrawCardAction (p, _) when p = player ->        
        prevCardCounts
        |> Map.map (fun card count ->
            if doCardsMatch discardPileTop card then
                0
            else
                min (count + 1) (maxCardCountsFromFullDeck.[card] - (countsFromOwnCardsDiscardPile |> Map.tryFind card |? 0)))
    | DrawCardAction (p, card) when p = viewpoint ->
        ensureCardCountIsNotAboveUpperBound prevCardCounts [card]
    | DrawCardAction (p, _) ->
        prevCardCounts

    | DrawAndPlayCardAction (p, c) when p = player ->
        let prevDiscardPileTop = prevState.DiscardPile.Head
        prevCardCounts
        |> Map.map (fun card count ->
            if doCardsMatch prevDiscardPileTop card then
                0
            else
                count)
    | DrawAndPlayCardAction (p, card) ->
        ensureCardCountIsNotAboveUpperBound prevCardCounts [removeColor card]

    | DrawCardsAndSkipAction (p, cards) when p = player ->
        let drawCount = cards.Length
        prevCardCounts
        |> Map.map (fun card count ->
            min (count + drawCount) (maxCardCountsFromFullDeck.[card] - (countsFromOwnCardsDiscardPile |> Map.tryFind card |? 0)))
    | DrawCardsAndSkipAction (p, cards) when p = viewpoint ->
        ensureCardCountIsNotAboveUpperBound prevCardCounts cards
    | DrawCardsAndSkipAction (p, _) ->
        prevCardCounts

let getCardCountingContext (context : CardCountingContext) (prevState : State) action (state : State) (viewpoint : Player) =
    Array.init state.Players.Length (fun i ->
        if i <> viewpoint then
            getCardCounts context prevState action state viewpoint i
        else
            Map.empty)
 
(*
für Player p:

initialisierung:
    - anzahlen = maxCardCountsFromFullDeck - countsFromOwnCardsDiscardPile

für jede Action in der History:
    - PlayCardAction p card:
          anzahl von card: -1 (sofern über 0)
      
    - DrawCardAction p:
          - ohne sofortiges Spielen:
                anzahlen von inkompatiblen Karten: +1 (sofern unter maxCardCountsFromFullDeck - countsFromOwnCardsDiscardPile)
                anzahlen von kompatiblen Karten: =0
          - mit sofortigem Spielen:
                anzahlen von kompatiblen Karten: =0

    - ForcedDrawCardAction p:
          anzahlen von allen Karten: +1 (sofern unter maxCardCountsFromFullDeck - countsFromOwnCardsDiscardPile)

    - PlayCardAction !p!player card:
          anzahl von card kann maximal maxCardCountsFromFullDeck.[card] - countsFromOwnCardsDiscardPile.[card] sein

    - PlayCardAction player card:
          n.a.

    - DrawCardAction !p!player:
          n.a.

    - DrawCardAction player:
          anzahl von card kann maximal maxCardCountsFromFullDeck.[card] - countsFromOwnCardsDiscardPile.[card] sein

    - ForcedDrawCardAction !p!player:
          n.a.

    - ForcedDrawCardAction player:
          anzahl von card kann maximal maxCardCountsFromFullDeck.[card] - countsFromOwnCardsDiscardPile.[card] sein
*)