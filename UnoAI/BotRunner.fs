module BotRunner

open System
open System.Threading
open System.Threading.Tasks

open Utils
open Card
open Game
open Bot

type GameResult = { Winner : Player; Score : int; GameLength : int }

let runBots (botsFactory : (Game * Player -> Bot) []) count =
    let numPlayers = botsFactory.Length

    let results : GameResult [] = Array.zeroCreate count

    let progress = ref 0
    
    Parallel.For(0, count, new Action<int>(fun it ->
    //for it in [0..count] do 
        let dealer = random().Next(numPlayers)

        let game = new Game(numPlayers, dealer)

        let bots = Array.init numPlayers (fun i -> botsFactory.[i](game, i))

        bots |> Array.iter (fun bot -> bot.Initialize game.State)

        let notifyActionPerformed prevState action state =
            bots |> Array.iter (fun bot -> bot.OnActionPerformed prevState action state)

        let notifyDrawAndSkipIfNeeded prevState card (p : Player) state =
            match card with
            | DrawTwo  _     -> 
                let playerWhoHadToDraw = (p + (if game.Direction = Clockwise then 1 else -1)) %% game.Players.Length
                notifyActionPerformed prevState (DrawCardsAndSkipAction (playerWhoHadToDraw, game.Players.[playerWhoHadToDraw] |> List.take 2)) state
            | WildDrawFour _ -> 
                let playerWhoHadToDraw = (p + (if game.Direction = Clockwise then 1 else -1)) %% game.Players.Length
                notifyActionPerformed prevState (DrawCardsAndSkipAction (playerWhoHadToDraw, game.Players.[playerWhoHadToDraw] |> List.take 4)) state
            | _              -> ()

        let mutable actionsPerformed = 0

        while game.Status = Playing && actionsPerformed < 1000 do
            let p = game.ActivePlayer
            let prevState = game.State
            match bots.[p].PerformAction() with
            | PlayCardBotAction card     ->
                game.PlayCard(card)
                notifyActionPerformed prevState (PlayCardAction (p, card)) game.State          
                notifyDrawAndSkipIfNeeded prevState card p game.State
            | DrawCardBotAction callback ->
                let mutable ca = Wild None
                let mutable ret = None
                game.DrawCard(fun card ->
                    ca <- card
                    ret <- callback card
                    ret)
                match ret with
                | None   -> notifyActionPerformed prevState (DrawCardAction (p, game.State.Players.[p].Head)) game.State
                | Some c -> notifyActionPerformed prevState (DrawAndPlayCardAction (p, c)) game.State
                            notifyDrawAndSkipIfNeeded prevState c p game.State
            actionsPerformed <- actionsPerformed + 1

        match game.Status with
        | Ended (winner, score) -> results.[it] <- { Winner = winner; Score = score; GameLength = actionsPerformed }
        | _                     -> failwith "Game timeout." 

        if count > 50000 then
            if Interlocked.Increment(progress) % 1000 = 0 then
                lock progress (fun () -> 
                    printf "%.0f%%" ((float progress.Value) / (float count) * 100.0)
                    Console.CursorLeft <- 0)
        )) |> ignore

    results