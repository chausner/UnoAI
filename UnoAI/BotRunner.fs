module BotRunner

open Utils
open Card
open Game
open Bot
open FSharp.Collections.ParallelSeq
open System
open System.Linq

type GameResult = {
    Winner : Player;
    Score : int;
    GameLength : int }

let runBots ruleSet (botsFactory : (Game * Player -> Bot) []) (randomizePlayerOrder : bool) timeout =
    let numPlayers = botsFactory.Length

    let dealer = Random.Shared.Next(numPlayers)

    let game = new Game(ruleSet, numPlayers, dealer)

    let botOrder = 
        if randomizePlayerOrder then
            [| 0..numPlayers-1 |] |> Seq.shuffle
        else
            [| 0..numPlayers-1 |]

    let bots = Array.init numPlayers (fun i -> botsFactory[botOrder[i]](game, i))

    bots |> Array.iter (fun bot -> bot.Initialize game.State)

    let notifyActionPerformed prevState action state =
        bots |> Array.iter (fun bot -> bot.OnActionPerformed prevState action state)

    let notifyDrawAndSkipIfNeeded prevState card (p : Player) state =
        let numCardsDrawn = 
            match card with
            | DrawTwo  _ -> 2
            | WildDrawFour _ -> 4
            | _ -> 0
        if numCardsDrawn <> 0 then
            let playerWhoHadToDraw = (p + (if game.Direction = Clockwise then 1 else -1)) %% game.NumPlayers
            notifyActionPerformed prevState (DrawCardsAndSkipAction (playerWhoHadToDraw, game.Players[playerWhoHadToDraw] |> List.take numCardsDrawn)) state

    let mutable actionsPerformed = 0

    while game.Status = Playing && actionsPerformed < timeout do
        let p = game.ActivePlayer
        let prevState = game.State
        match bots[p].PerformAction() with
        | PlayCardBotAction card     ->
            game.PlayCard(card)
            notifyActionPerformed prevState (PlayCardAction (p, card)) game.State          
            notifyDrawAndSkipIfNeeded prevState card p game.State
        | DrawCardBotAction callback ->
            let mutable cardPlayed = None
            game.DrawCard(fun card ->
                cardPlayed <- callback card
                cardPlayed)
            match cardPlayed with
            | None   -> notifyActionPerformed prevState (DrawCardAction (p, game.State.Players[p].Head)) game.State
            | Some c -> notifyActionPerformed prevState (DrawAndPlayCardAction (p, c)) game.State
                        notifyDrawAndSkipIfNeeded prevState c p game.State
        actionsPerformed <- actionsPerformed + 1

    match game.Status with
    | Ended (winner, score) -> { Winner = botOrder[winner]; Score = score; GameLength = actionsPerformed }
    | _                     -> failwith "Game timeout." 

let runBotsBatch ruleSet (botsFactory : (Game * Player -> Bot) []) (randomizePlayerOrder : bool) timeout count =
#if !DEBUG
    PSeq.init count (fun _ ->
#else
    Seq.init count (fun _ ->
#endif
        runBots ruleSet botsFactory randomizePlayerOrder timeout)
#if !DEBUG
    |> PSeq.withMergeOptions ParallelMergeOptions.NotBuffered
#endif

type GameStats = 
    { mutable NumGames: int;
      mutable NumGamesWon: int [];      
      mutable TotalPoints: int []; }

let initStats numPlayers : GameStats =
    { NumGames = 0;
      NumGamesWon = Array.zeroCreate numPlayers;
      TotalPoints = Array.zeroCreate numPlayers }

let updateStats (stats: GameStats) (result: GameResult) =
    stats.NumGames <- stats.NumGames + 1
    stats.NumGamesWon[result.Winner] <- stats.NumGamesWon[result.Winner] + 1
    stats.TotalPoints[result.Winner] <- stats.TotalPoints[result.Winner] + result.Score

let printStats (stats : GameStats) (bots : (Game.Game * Game.Player -> Bot) []) =
    let getConfidenceInterval (k : int) (n : int) =
        let z = 1.96 // 95% confidence level
        let µ = float k / float n
        let σ = sqrt (µ * (1.0 - µ))
        z * σ / (sqrt (float n))

    printfn "ID  Player                    Win rate  Avg. points"

    for player = 0 to bots.Length - 1 do
        let playerName = (bots[player](new Game.Game(defaultRuleSet, 2, 0), 0)).GetType().Name
        let winRate = (float stats.NumGamesWon[player]) / (float stats.NumGames)
        let winRateInterval = getConfidenceInterval stats.NumGamesWon[player] stats.NumGames
        let averagePoints = (float stats.TotalPoints[player]) / (float stats.NumGames)
        printfn "%-3i %-18s %7.3f%%±%.3f%%  %11.1f" player playerName (winRate * 100.0) (winRateInterval * 100.0) averagePoints
        