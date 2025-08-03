module GreedyBot

open Card
open Game
open Bot
open Utils
open BotUtils

type GreedyBotSettings =
    { ChooseMostCommonColor: bool }

type GreedyBot(game: Game, player: Player, settings: GreedyBotSettings) =
    inherit Bot()

    let getMostCommonColor () =
        game.Players[player]
        |> Seq.choose getCardColor
        |> Seq.countBy id
        |> Seq.shuffle
        |> Seq.tryMaxBy snd
        |> Option.map fst

    let chooseColor () =
        if settings.ChooseMostCommonColor then
            getMostCommonColor () |? getRandomColor ()
        else
            getRandomColor ()

    override self.PerformAction() =
        let playableCards =
            game.Players[player]
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard
            |> Seq.toList

        if not (playableCards |> List.isEmpty) then
            let playedCard =
                playableCards
                |> Seq.pickMaxBy getCardScore
                |> List.chooseRandom
                |> fun card -> chooseColorIfNeeded card chooseColor

            PlayCardBotAction playedCard
        else
            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard chooseColor))

    static member Factory(settings: GreedyBotSettings) =
        fun game player -> new GreedyBot(game, player, settings) :> Bot

    static member DefaultSettings = { ChooseMostCommonColor = true }