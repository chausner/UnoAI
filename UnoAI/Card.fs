module Card

type CardColor =
    | Red
    | Green
    | Blue
    | Yellow

type CardRank = int

type Card =
    | StandardCard of CardRank * CardColor
    | Skip of CardColor
    | DrawTwo of CardColor
    | Reverse of CardColor
    | Wild of CardColor option
    | WildDrawFour of CardColor option

let fullCardDeck =
    [ for n = 0 to 9 do
          for c in [| Red; Green; Blue; Yellow |] do
              yield StandardCard(n, c)
              if n <> 0 then
                  yield StandardCard(n, c)

      for c in [| Red; Green; Blue; Yellow |] do
          for _ = 1 to 2 do
              yield Skip c
              yield DrawTwo c
              yield Reverse c 
              
      for _ = 1 to 4 do
          yield Wild None
          yield WildDrawFour None ]

let getCardColor card =
    match card with
    | StandardCard (_, c)   
    | Skip c                
    | DrawTwo c             
    | Reverse c             
    | Wild (Some c)         
    | WildDrawFour (Some c) -> Some c
    | _                     -> None

let chooseColor card color =
    match card with
    | Wild None         -> Wild (Some color)
    | WildDrawFour None -> WildDrawFour (Some color)
    | _                 -> invalidArg "card" "The specified card must be \"Wild\" or \"Wild Draw Four\"."

let removeColor card =
    match card with
    | Wild _         -> Wild None
    | WildDrawFour _ -> WildDrawFour None
    | _              -> card

let doCardsMatch oldCard newCard =
    match oldCard, newCard with
    | StandardCard (n1, _), StandardCard (n2, _) when n1 = n2 -> true
    | Skip _,               Skip _  
    | DrawTwo _,            DrawTwo _
    | Reverse _,            Reverse _                          
    | _,                    Wild _
    | _,                    WildDrawFour _                    -> true
    | _,                    _                                 ->
        match getCardColor oldCard, getCardColor newCard with
        | None, _    -> invalidArg "oldCard" "Card needs to have a color assigned."
        | _,    None -> invalidArg "newCard" "Card needs to have a color assigned."
        | c1,   c2   -> c1 = c2

let getCardScore card =
    match card with
    | StandardCard (n, _)            -> int n
    | Skip _ | DrawTwo _ | Reverse _ -> 20
    | Wild _ | WildDrawFour _        -> 50