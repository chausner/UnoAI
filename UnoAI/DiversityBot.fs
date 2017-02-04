module DiversityBot

open Card
open Game
open Bot
open Utils

type DiversityBot(game : Game, player : Player) =
    inherit Bot()

    let getRandomColor() =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

    let chooseColorIfNeeded card color =
        match card with
        | Wild None         
        | WildDrawFour None -> chooseColor card (color())
        | _                 -> card

    let getCardDiversityScore card =
        match card with
        | Wild None | WildDrawFour None -> -8
        | _ ->
            game.Players.[player]
            |> Seq.except [Wild None; WildDrawFour None]
            |> Seq.filter (doCardsMatch card)
            |> Seq.length
            |> (*) -1

    let getMostCommonColor() =
        game.Players.[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let chooseColor() =
        // getRandomColor()
        getMostCommonColor() |? getRandomColor()

    let pickMaxBy projection list =
        match list with
        | [] | [_] -> list
        | _        ->
            let maxValue = list |> Seq.map projection |> Seq.max
            List.filter (fun x -> projection x = maxValue) list

    override self.PerformAction() =
        let playableCards =            
            game.Players.[player]  
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard
            |> Seq.toList

        if not (playableCards |> List.isEmpty) then
            let playedCard =
                playableCards
                |> pickMaxBy getCardDiversityScore           
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
//            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard chooseColor))
            DrawCardBotAction (fun drawnCard -> 
                match drawnCard with
                | Wild _ | WildDrawFour _ when game.Players.[player].Length > 2 -> None
                | _ -> Some (chooseColorIfNeeded drawnCard chooseColor))