module RandomBot

open Card
open Game
open Bot
open Utils

type RandomBot(game: Game, player: Player) =
    inherit Bot()

    let chooseColorIfNeeded card color =
        match card with
        | Wild None
        | WildDrawFour None -> chooseColor card (color ())
        | _                 -> card

    let getRandomColor () =
        [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

    override self.PerformAction() =
        let playableCards =
            game.Players[player]
            |> Seq.filter game.CanPlayCard
            |> Seq.toArray

        if not (playableCards |> Array.isEmpty) then
            let playedCard =
                playableCards
                |> Array.chooseRandom
                |> fun card -> chooseColorIfNeeded card getRandomColor

            PlayCardBotAction playedCard
        else
            DrawCardBotAction (fun drawnCard -> Some (chooseColorIfNeeded drawnCard getRandomColor))

    static member Factory() =
        fun game player -> new RandomBot(game, player) :> Bot
