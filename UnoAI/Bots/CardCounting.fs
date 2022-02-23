module CardCounting

open Card
open Game
open Utils

let private fullDeckCardCounts =
    fullCardDeck
    |> Seq.countBy id
    |> Map.ofSeq

type CardCountingContext = Map<Card, int> []

let private subtractCounts (counts1: Map<Card, int>) (counts2: Map<Card, int>) =
    counts1
    |> Map.toSeq
    |> Seq.map (fun (card, count) -> card, count - (counts2 |> Map.tryFind card |? 0))
    |> Map.ofSeq

let private getCountsFromOwnCardsDiscardPile (state: State) (viewpoint: Player) =
    Seq.append state.Players[viewpoint] state.DiscardPile
    |> Seq.countBy removeColor
    |> Map.ofSeq

let private getInitialCardCounts (initialState: State) viewpoint =
    getCountsFromOwnCardsDiscardPile initialState viewpoint
    |> subtractCounts fullDeckCardCounts

let initCardCountingContext (initialState: State) viewpoint =
    let context = Array.create initialState.Players.Length (getInitialCardCounts initialState viewpoint)
    context[viewpoint] <- Map.empty
    context

let private getCardCounts (prevContext: CardCountingContext) (prevState: State) action (state: State) viewpoint (player: Player) =
    let prevCardCounts = prevContext[player]
    let countsFromOwnCardsDiscardPile = getCountsFromOwnCardsDiscardPile state viewpoint
    let discardPileTop = state.DiscardPile.Head

    let rec ensureCardCountIsNotAboveUpperBound (cardCounts: Map<Card, int>) cards =
        match cards with
        | [] -> cardCounts
        | card :: tail ->
            let upperBound = fullDeckCardCounts[card] - (countsFromOwnCardsDiscardPile |> Map.tryFind card |? 0)
            let x =
                if cardCounts[card] > upperBound then
                    cardCounts |> Map.add card upperBound
                else
                    cardCounts
            ensureCardCountIsNotAboveUpperBound x tail

    match action with
    | PlayCardAction (p, card) when p = player ->
        prevCardCounts
        |> Map.add (removeColor card) (max (prevCardCounts[removeColor card] - 1) 0)
    | PlayCardAction (p, card) when p = viewpoint ->
        prevCardCounts
    | PlayCardAction (p, card) ->
        ensureCardCountIsNotAboveUpperBound prevCardCounts [ removeColor card ]

    | DrawCardAction (p, _) when p = player ->
        prevCardCounts
        |> Map.map (fun card count ->
            if doCardsMatch discardPileTop card then
                0
            else
                min (count + 1) (fullDeckCardCounts[card] - (countsFromOwnCardsDiscardPile |> Map.tryFind card |? 0)))
    | DrawCardAction (p, card) when p = viewpoint ->
        ensureCardCountIsNotAboveUpperBound prevCardCounts [ card ]
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
        ensureCardCountIsNotAboveUpperBound prevCardCounts [ removeColor card ]

    | DrawCardsAndSkipAction (p, cards) when p = player ->
        let drawCount = cards.Length
        prevCardCounts
        |> Map.map (fun card count ->
            min (count + drawCount) (fullDeckCardCounts[card] - (countsFromOwnCardsDiscardPile |> Map.tryFind card |? 0)))
    | DrawCardsAndSkipAction (p, cards) when p = viewpoint ->
        ensureCardCountIsNotAboveUpperBound prevCardCounts cards
    | DrawCardsAndSkipAction (p, _) ->
        prevCardCounts

let updateCardCountingContext (context: CardCountingContext) (prevState: State) action (state: State) (viewpoint: Player) =
    Array.init state.Players.Length (fun player ->
        if player <> viewpoint then
            getCardCounts context prevState action state viewpoint player
        else
            Map.empty)