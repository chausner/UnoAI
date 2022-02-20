﻿module GreedyBot

open Card
open Game
open Bot
open Utils

/// <summary>
/// Bot that makes decisions in a greedy manner (i.e. chooses the option that gives the highest immediate benefit).
///
/// * Out of all playable cards, the card with the highest score is chosen.
/// * When a drawn card can be played, it is always played.
/// * When a Wild or WildDrawFour is played, the color that is most common in the player's hand is chosen.
/// </summary>
type GreedyBot(game : Game, player : Player, chooseMostCommonColor : bool) =
    inherit Bot()

    let chooseColorIfNeeded card color =
        match card with
        | Wild None         
        | WildDrawFour None -> chooseColor card (color())
        | _                 -> card

    let getMostCommonColor() =
        game.Players[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let getRandomColor() =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

    let chooseColor() =
        if chooseMostCommonColor then
            getMostCommonColor() |? getRandomColor()
        else
            getRandomColor()

    let pickMaxBy projection list =
        match list with
        | [] | [_] -> list
        | _        ->
            let maxValue = list |> Seq.map projection |> Seq.max
            list |> List.filter (fun x -> projection x = maxValue)

    override self.PerformAction() =
        let playableCards =            
            game.Players[player]  
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard
            |> Seq.toList

        if not (playableCards |> List.isEmpty) then
            let playedCard =
                playableCards
                |> pickMaxBy getCardScore
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard chooseColor))

    static member Factory(chooseMostCommonColor : bool) = 
        fun (game, player) -> new GreedyBot(game, player, chooseMostCommonColor) :> Bot