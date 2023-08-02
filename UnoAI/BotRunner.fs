module BotRunner

open Utils
open Card
open Game
open Bot
open FSharp.Collections.ParallelSeq
open System
open System.Diagnostics
open System.Linq

type GameResult =
    { Winner: Player
      Score: int
      GameLength: int }

type BotFactory = Game -> Player -> Bot

let runBots ruleSet (botsFactory: BotFactory []) (randomizePlayerOrder: bool) timeout =
    let numPlayers = botsFactory.Length

    let dealer = Random.Shared.Next(numPlayers)

    let game = new Game(ruleSet, numPlayers, dealer)

    let botOrder =
        if randomizePlayerOrder then
            [| 0 .. numPlayers - 1 |] |> Seq.shuffle
        else
            [| 0 .. numPlayers - 1 |]

    let bots = Array.init numPlayers (fun i -> botsFactory[botOrder[i]] game i)

    bots |> Array.iter (fun bot -> bot.Initialize game.State)

    let notifyActionPerformed prevState action state =
        bots |> Array.iter (fun bot -> bot.OnActionPerformed prevState action state)

    let notifyDrawAndSkipIfNeeded prevState card (p: Player) state =
        let numCardsDrawn =
            match card with
            | DrawTwo _      -> 2
            | WildDrawFour _ -> 4
            | _              -> 0

        if numCardsDrawn <> 0 then
            let playerWhoHadToDraw = (p + (if game.Direction = Clockwise then 1 else -1)) %% game.NumPlayers
            let cardsDrawn = game.Players[playerWhoHadToDraw] |> List.take numCardsDrawn
            notifyActionPerformed prevState (DrawCardsAndSkipAction (playerWhoHadToDraw, cardsDrawn)) state

    let mutable actionsPerformed = 0

    while game.Status = Playing && actionsPerformed < timeout do
        let p = game.ActivePlayer
        let prevState = game.State

        match bots[p].PerformAction() with
        | PlayCardBotAction card ->
            game.PlayCard(card)
            notifyActionPerformed prevState (PlayCardAction(p, card)) game.State
            notifyDrawAndSkipIfNeeded prevState card p game.State
        | DrawCardBotAction callback ->
            let mutable cardPlayed = None
            game.DrawCard (fun card ->
                cardPlayed <- callback card
                cardPlayed)
            match cardPlayed with
            | None ->
                notifyActionPerformed prevState (DrawCardAction (p, game.State.Players[p].Head)) game.State
            | Some c ->
                notifyActionPerformed prevState (DrawAndPlayCardAction (p, c)) game.State
                notifyDrawAndSkipIfNeeded prevState c p game.State

        actionsPerformed <- actionsPerformed + 1

    match game.Status with
    | Ended (winner, score) ->
        { Winner = botOrder[winner]
          Score = score
          GameLength = actionsPerformed }
    | _ -> failwith "Game timeout."

let runBotsBatch ruleSet (botsFactory: BotFactory []) (randomizePlayerOrder: bool) timeout count =
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
    { mutable NumGames: int
      mutable NumGamesWon: int []
      mutable TotalPoints: int [] }

let initStats numPlayers : GameStats =
    { NumGames = 0
      NumGamesWon = Array.zeroCreate numPlayers
      TotalPoints = Array.zeroCreate numPlayers }

let updateStats (stats: GameStats) (result: GameResult) =
    stats.NumGames <- stats.NumGames + 1
    stats.NumGamesWon[result.Winner] <- stats.NumGamesWon[result.Winner] + 1
    stats.TotalPoints[result.Winner] <- stats.TotalPoints[result.Winner] + result.Score

let private printfncl (format: Printf.TextWriterFormat<'T>): 'T =
    // ANSI escape code to clear from current cursor to end of line
    let clearLine = "\x1B[0K"
    Printf.kfprintf (fun _ -> printfn "%s" clearLine) Console.Out format

let private printStats (stats: GameStats) (bots: BotFactory []) =
    let getConfidenceInterval (k: int) (n: int) =
        let z = 1.96 // 95% confidence level
        let µ = float k / float n
        let σ = sqrt (µ * (1.0 - µ))
        z * σ / (sqrt (float n))

    printfncl "%s" "ID  Player                    Win rate  Avg. points"

    for player = 0 to bots.Length - 1 do
        let playerName = (bots[player] (new Game.Game(defaultRuleSet, 2, 0)) 0).GetType().Name
        let winRate = (float stats.NumGamesWon[player]) / (float stats.NumGames)
        let winRateInterval = getConfidenceInterval stats.NumGamesWon[player] stats.NumGames
        let averagePoints = (float stats.TotalPoints[player]) / (float stats.NumGames)
        printfncl "%-3i %-18s %7.3f%%±%.3f%%  %11.1f" player playerName (winRate * 100.0) (winRateInterval * 100.0) averagePoints

let runBatchAndPrintStats ruleSet (bots: BotFactory []) randomizePlayOrder timeout numGames =
    enableVirtualTerminalProcessing ()
    printfn "Games: %i" numGames
    printfn "Game timeout: %i" timeout
    let cursorPos = Console.GetCursorPosition()

    let restoreCursor () =
        let struct (cursorLeft, cursorTop) = cursorPos
        Console.SetCursorPosition(cursorLeft, cursorTop)

    let stats = initStats bots.Length
    let mutable lastProgressUpdate: TimeSpan option = None

    let printProgress (stopwatch: Stopwatch) (refreshInterval: TimeSpan) =
        if lastProgressUpdate.IsNone || stopwatch.Elapsed - lastProgressUpdate.Value >= refreshInterval then
            let gamesPerSecond = 
                if stopwatch.Elapsed.TotalSeconds = 0 then
                    0.0
                else
                    float stats.NumGames / stopwatch.Elapsed.TotalSeconds
            printfncl "Games per second: %i" (int gamesPerSecond)
            printfncl "Progress: %.0f%%" ((float stats.NumGames) / (float numGames) * 100.0)
            let eta = 
                match stats.NumGames with
                | 0 -> TimeSpan.Zero
                | _ -> float (numGames - stats.NumGames) / float stats.NumGames * stopwatch.Elapsed            
            printfncl "Elapsed: %s (ETA: %s)" (stopwatch.Elapsed |> formatTimeSpan) (eta |> formatTimeSpan)
            printStats stats bots
            restoreCursor ()
            lastProgressUpdate <- Some stopwatch.Elapsed

    let _, elapsed =
        stopwatch (fun stopwatch ->
            runBotsBatch ruleSet bots randomizePlayOrder timeout numGames
            |> Seq.iter (fun result ->
                updateStats stats result
                printProgress stopwatch (TimeSpan.FromSeconds(1))))

    let gamesPerSecond = 
        if elapsed.TotalSeconds = 0 then
            0.0
        else
            float stats.NumGames / elapsed.TotalSeconds
    printfncl "Games per second: %i" (int gamesPerSecond)
    printfncl "Progress: 100%%"
    printfncl "Elapsed: %s" (elapsed |> formatTimeSpan)
    printStats stats bots