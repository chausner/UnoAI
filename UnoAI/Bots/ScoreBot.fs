module ScoreBot

open Card
open Game
open Bot
open Utils

type ScoreBotSettings =
    { Weights: float [] }

type private GameView =
    { OwnCards: Card list
      PlayerCardCounts: int []
      ActivePlayer: Player
      Direction: Direction
      DiscardPile: Card list }

type ScoreBot(game: Game, player: Player, settings: ScoreBotSettings) =
    inherit Bot()

    let cardScoringFunction card =
        match card with
        | StandardCard (_, _) -> settings.Weights[0]
        | Reverse _ when game.RuleSet.TwoPlayerReverseIsSkip && game.NumPlayers = 2 -> settings.Weights[2]
        | Reverse _           -> settings.Weights[1]
        | Skip _              -> settings.Weights[2]
        | DrawTwo _           -> settings.Weights[3]
        | Wild _              -> settings.Weights[4]
        | WildDrawFour _      -> settings.Weights[5]

    let scoreFunction (view: GameView) =
        view.OwnCards |> Seq.sumBy cardScoringFunction

    let getAllColorOptions card =
        match card with
        | Wild None         -> allCardColors |> Seq.map (fun c -> Wild (Some c))
        | WildDrawFour None -> allCardColors |> Seq.map (fun c -> WildDrawFour (Some c))
        | _                 -> Seq.singleton card

    let getView () =
        { OwnCards = game.Players[player]
          PlayerCardCounts = game.Players |> Array.map List.length
          ActivePlayer = game.ActivePlayer
          Direction = game.Direction
          DiscardPile = game.DiscardPile }

    let applyPlayCardAction card view =
        let removeCardFromHand card hand =
            let i = hand |> List.findIndex ((=) (removeColor card))
            hand |> List.removeAt i

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

            (view.ActivePlayer + delta) %% game.NumPlayers

        let activePlayer' =
            match card with
            | StandardCard (_, _) -> advance 1
            | Skip _              -> advance 2
            | DrawTwo _           -> advance 2
            | Reverse _ when game.RuleSet.TwoPlayerReverseIsSkip && game.NumPlayers = 2 -> advance 2
            | Reverse _           -> advance 1
            | Wild _              -> advance 1
            | WildDrawFour _      -> advance 2

        let playerCardCounts' = Array.copy view.PlayerCardCounts

        playerCardCounts'[player] <- playerCardCounts'[player] - 1

        match card with
        | DrawTwo _      -> playerCardCounts'[advance 1] <- playerCardCounts'[advance 1] + 2
        | WildDrawFour _ -> playerCardCounts'[advance 1] <- playerCardCounts'[advance 1] + 4
        | _              -> ()

        { OwnCards = ownCards'
          PlayerCardCounts = playerCardCounts'
          ActivePlayer = activePlayer'
          Direction = direction'
          DiscardPile = discardPile' }

    let applyAdvance view =
        let nextPlayer: Player =
            match view.Direction with
            | Clockwise        -> (view.ActivePlayer + 1) %% game.NumPlayers
            | Counterclockwise -> (view.ActivePlayer - 1) %% game.NumPlayers

        { view with ActivePlayer = nextPlayer }

    let playDrawnCardCallback drawnCard =
        let view = getView ()

        let options =
            seq {
                yield!
                    getAllColorOptions drawnCard
                    |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, view |> applyAdvance |> scoreFunction
            }

        options |> Seq.maxBy snd |> fst

    let averageDrawCardScore = fullCardDeck |> Seq.averageBy cardScoringFunction

    override self.PerformAction() =
        let playableCards =
            game.Players[player]
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard

        let view = getView ()

        let options =
            seq {
                yield!
                    playableCards
                    |> Seq.collect getAllColorOptions
                    |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, (view |> scoreFunction) + averageDrawCardScore
            }

        let bestOption = options |> Seq.maxBy snd |> fst

        match bestOption with
        | Some card -> PlayCardBotAction card
        | None      -> DrawCardBotAction playDrawnCardCallback

    static member Factory(settings: ScoreBotSettings) =
        fun game player -> new ScoreBot(game, player, settings) :> Bot

    static member DefaultSettingsWinRate =
        { Weights = [| -0.168; -0.163; -0.190; -0.227; 0.048; -0.035 |] } // weights optimized on win rate against 3 other random bots
    static member DefaultSettingsAvgPoints =
        { Weights = [| -0.189; -0.153; -0.206; -0.193; 0.015; 0.023 |] } // weights optimized on average points against 3 other random bots
