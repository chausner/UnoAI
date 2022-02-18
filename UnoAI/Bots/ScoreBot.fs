module ScoreBot

open Card
open Game
open Bot
open Utils
open BotRunner
open RandomBot
open System
open System.Globalization
open System.IO

type private GameView = {
    OwnCards : Card list;
    PlayerCardCounts: int [];
    ActivePlayer : Player; 
    Direction : Direction;
    DiscardPile : Card list }

type ScoreBot(game : Game, player : Player, weights : float []) =
    inherit Bot()

    let cardScoringFunction card =
        match card with                     
        | StandardCard (_, _) -> weights[0]
        | Reverse _           -> weights[1]
        | Skip _              -> weights[2]
        | DrawTwo _           -> weights[3]
        | Wild _              -> weights[4]
        | WildDrawFour _      -> weights[5]

    let scoreFunction (view : GameView) =  
        view.OwnCards |> Seq.sumBy cardScoringFunction

    let getAllColorOptions card =
        let colors = [| Red; Green; Blue; Yellow |]
        match card with
        | Wild None         -> colors |> Seq.map (fun c -> Wild (Some c))
        | WildDrawFour None -> colors |> Seq.map (fun c -> WildDrawFour (Some c))
        | _                 -> Seq.singleton card

    let getView() = {
        OwnCards = game.Players[player];
        PlayerCardCounts = game.Players |> Array.map List.length;
        ActivePlayer = game.ActivePlayer;
        Direction = game.Direction;
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
            (view.ActivePlayer + delta) %% view.PlayerCardCounts.Length

        let activePlayer' = 
            match card with
            | StandardCard (_, _) -> advance 1
            | Skip _              -> advance 2
            | DrawTwo _           -> advance 2
            | Reverse _           -> advance 1
            | Wild _              -> advance 1
            | WildDrawFour _      -> advance 2

        let playerCardCounts' = Array.copy view.PlayerCardCounts
        
        playerCardCounts'[player] <- playerCardCounts'[player] - 1

        match card with
        | DrawTwo _      -> playerCardCounts'[advance 1] <- playerCardCounts'[advance 1] + 2
        | WildDrawFour _ -> playerCardCounts'[advance 1] <- playerCardCounts'[advance 1] + 4
        | _              -> ()

        { OwnCards = ownCards';
          PlayerCardCounts = playerCardCounts';
          ActivePlayer = activePlayer';
          Direction = direction';
          DiscardPile = discardPile' }

    let applyDrawCardAction (view : GameView) = 
        let advance steps : Player =
            let delta =
                match view.Direction with
                | Clockwise        -> steps
                | Counterclockwise -> -steps
            (view.ActivePlayer + delta) %% view.PlayerCardCounts.Length

        let playerCardCounts' = Array.copy view.PlayerCardCounts
        
        playerCardCounts'[player] <- playerCardCounts'[player] + 1

        let cardDrawn = fullCardDeck |> List.chooseRandom

        { OwnCards = cardDrawn :: view.OwnCards;
          PlayerCardCounts = playerCardCounts';
          ActivePlayer = advance 1;
          Direction = view.Direction;
          DiscardPile = view.DiscardPile }

    let applyAdvance view =
        let nextPlayer : Player =
            match view.Direction with
            | Clockwise        -> (view.ActivePlayer + 1) %% view.PlayerCardCounts.Length
            | Counterclockwise -> (view.ActivePlayer - 1) %% view.PlayerCardCounts.Length
        { view with ActivePlayer = nextPlayer }

    let playDrawnCardCallback drawnCard =
        let view = getView()

        let options =
            seq {
                yield! getAllColorOptions drawnCard
                       |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                yield None, view |> applyAdvance |> scoreFunction }

        options |> Seq.maxBy snd |> fst

    let averageDrawCardScore = fullCardDeck |> Seq.averageBy cardScoringFunction       

    override self.PerformAction() =
        let playableCards =            
            game.Players[player]  
            |> Seq.distinct
            |> Seq.filter game.CanPlayCard

        let view = getView()

        let options = 
            seq {
                yield! playableCards
                       |> Seq.collect getAllColorOptions
                       |> Seq.map (fun card -> Some card, view |> applyPlayCardAction card |> scoreFunction)

                //yield None, view |> applyDrawCardAction |> scoreFunction
                yield None, (view |> scoreFunction) + averageDrawCardScore
            }

        let bestOption = options |> Seq.maxBy snd |> fst

        match bestOption with
        | Some card -> PlayCardBotAction card
        | None      -> DrawCardBotAction playDrawnCardCallback

    static member Factory(weights : float []) =
        fun (game, player) -> new ScoreBot(game, player, weights) :> Bot

    static member WeightsWinRate = [| -0.90; -0.64; -0.62; -0.81; 0.55; 0.48; 0.14; 0.13 |] // weights optimized on win rate against 3 other random bots
    static member WeightsAvgPoints = [| -0.81; -0.45; -0.85; -0.52; 0.14; 0.70; 0.08; 0.06 |] // weights optimized on average points against 3 other random bots
    static member WeightsWinRateNew = [| -0.168; -0.163; -0.190; -0.227; 0.048; -0.035 |] // weights optimized on win rate against 3 other random bots

let optimizeWeights() =
    let ruleSet = defaultRuleSet
    let numPlayers = 4
    let numGames = 30_000

    let printScoring (scoring : float []) =
        String.concat " " (scoring |> Seq.map (fun x -> x.ToString("F3", CultureInfo.InvariantCulture).PadLeft(6)))

    let ranges = [ (-0.26, -0.17); (-0.26, -0.17); (-0.22, -0.08); (-0.32, -0.10); (-0.35, -0.20); (-0.13, 0.14); (-0.12, 0.08) ]

    let getRandomInRanges ranges =
        ranges
        |> Seq.map (fun (l, h) -> Random.Shared.NextDouble() * (h - l) + l)
        |> Seq.toArray

    let getRandomNear scoring margin =
        scoring |> Array.map (fun x -> x * (1.0 + Random.Shared.NextDouble() * 2.0 * margin - margin))

    let normalizeScoring scoring =
        let s = scoring |> Seq.map abs |> Seq.sum
        scoring |> Array.map (fun x -> x / s)

    let pid = System.Environment.ProcessId
    use output = File.CreateText(@$"C:\Users\chris\Desktop\UnoAI\run4\{pid}.csv")
    output.AutoFlush <- true

    //for scoring in Seq.initInfinite (fun _ -> Array.init 6 (fun _ -> Random.Shared.NextDouble() * 2.0 - 1.0)) do
    for scoring in Seq.initInfinite (fun _ -> getRandomInRanges ranges) |> Seq.map normalizeScoring do
    //for scoring in Seq.initInfinite (fun _ -> getRandomNear [| -0.359;0.431;0.041;0.062;0.108 |] 0.2) |> Seq.map normalizeScoring do
    //for scoring in [0.00001..0.01..0.2] |> Seq.map (fun w -> Array.append scoring' [| w |]) do
    //for param in [-20..1] |> List.rev do
        let scoring = scoring |> Seq.map float |> Seq.toArray
        let bots =     
            ScoreBot.Factory(scoring) :: (List.replicate (numPlayers - 1) (RandomBot.Factory()))
            |> Seq.toArray
            
        try
            let stats = initStats numPlayers

            runBotsBatch ruleSet bots true 1000 numGames
            |> Seq.iter (updateStats stats)

            let winRate = (float stats.NumGamesWon[0]) / (float stats.NumGames)
            let averagePoints = (float stats.TotalPoints[0]) / (float stats.NumGames)        
            printfn "%s  %.4f %.4f" (printScoring scoring) winRate averagePoints
            output.WriteLine(String.Join(';', (Array.append scoring [| winRate; averagePoints |])))            
        with
        | e -> () // printfn "error"