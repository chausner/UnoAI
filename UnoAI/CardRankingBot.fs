module CardRankingBot

open Card
open Game
open Bot
open Utils

type CardRankingBot(game : Game, player : Player) =
    inherit Bot()

    let scoringFunction card = // winrate optimized:    5 3 4 6 1 2 (0.342724 40.7 50.3)
        match card with        // avg points optimized: 5 3 6 4 2 1
        | StandardCard (n, _) -> 5
        | Reverse _           -> 3
        | Skip _              -> 4
        | DrawTwo _           -> 6
        | Wild _              -> 1
        | WildDrawFour _      -> 2

    let chooseColorIfNeeded card color =
        match card with
        | Wild None         
        | WildDrawFour None -> chooseColor card (color())
        | _                 -> card

    let getMostCommonColor() =
        game.Players.[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let getRandomColor() =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

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
                |> pickMaxBy scoringFunction           
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
//            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard chooseColor))
            DrawCardBotAction (fun drawnCard -> 
                match drawnCard with
                | Wild _ | WildDrawFour _ when game.Players.[player].Length > 2 -> None
                | _ -> Some (chooseColorIfNeeded drawnCard chooseColor))