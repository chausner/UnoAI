open Utils
open Bot
open BotRunner
open RandomBot
open GreedyBot
open CardRankingBot
open DiversityBot
open ScoreBot
open System.Globalization

let botcast (bot : #Bot) = bot :> Bot

let singleRun() =
    let numGames = 100000
    
    let bots =        
        [|//(fun (game, player) -> new ScoreBot(game, player, [| -0.90; -0.64; -0.62; -0.81; 0.55; 0.48; 0.14; 0.13; 0.1 |]) :> Bot)
          RandomBot >> botcast;           
          GreedyBot >> botcast; 
          CardRankingBot >> botcast; 
          DiversityBot >> botcast;           
         |] 

    let gameResults, elapsed =
        stopwatch (fun () -> runBots bots numGames)

    printfn "Elapsed: %.2fs" elapsed.TotalSeconds
    printfn ""

    let getNormalApproxInterval (k : int) (n : int) =
        let p = float k / float n
        let z = 1.96 // 5% confidence
        z * sqrt (p * (1.0 - p) / float n)

    let stats =
        gameResults
        |> Seq.groupBy (fun gameResult -> gameResult.Winner)
        |> Seq.sortBy fst
        |> Seq.map (fun (winner, gameResults) ->           
            let winRate = (gameResults |> Seq.length |> float) / (float numGames)
            let averagePoints = gameResults |> Seq.map (fun gameResult -> gameResult.Score) |> Seq.map float |> Seq.average
            let averageGameLength = gameResults |> Seq.averageBy (fun gameResult -> float gameResult.GameLength)
            let winRateInterval = getNormalApproxInterval (gameResults |> Seq.length) numGames
            winner, winRate, averagePoints, averageGameLength, winRateInterval)

    printfn "ID  Player          Win rate        Avg. points  Avg. game length"

    stats
    |> Seq.iter (fun (winner, winRate, averagePoints, averageGameLength, winRateInterval) -> 
        let winnerName = (bots.[winner](new Game.Game(2, 0), 0)).GetType().Name
        let averagePointsPerGame = winRate * averagePoints
        printfn "%-3i %-15s %.3f%%±%.3f%%  %.1f         %.1f" winner winnerName (winRate * 100.0) (winRateInterval * 100.0) averagePointsPerGame averageGameLength)
        
let scoringEvaluation() =
    let numPlayers = 4
    let numGames = 10000

    let rec getPermutations n : int list seq =
        match n with
        | 0 -> seq { yield [ ] }
        | _ -> let perms = getPermutations (n - 1)
               seq {
                   for p in perms do
                       for i = 0 to p.Length do
                           yield (p |> List.truncate i) @ [ n ] @ (p |> List.skip i) }

    let printScoring (scoring : float []) =
        //String.concat " " (scoring |> Seq.map string)
        String.concat " " (scoring |> Seq.map (fun x -> x.ToString("F3", CultureInfo.InvariantCulture).PadLeft(6)))

    let ranges = [ (-0.45, -0.21); (0.02, 0.43); (0.0, 0.33); (0.06, 0.23); (-0.15, 0.18) ]

    let getRandomInRanges ranges =
        ranges
        |> Seq.map (fun (l, h) -> random().NextDouble() * (h - l) + l)
        |> Seq.toArray

    let getRandomNear scoring =
        scoring |> Array.map (fun x -> x * (random().NextDouble() * 0.4 + 0.8))

    let normalizeScoring scoring =
        let s = scoring |> Seq.map abs |> Seq.sum
        scoring |> Array.map (fun x -> x / s)

    //for scoring in getPermutations 6 do
    //for scoring in Seq.initInfinite (fun _ -> Array.init 5 (fun _ -> random().NextDouble() * 2.0 - 1.0)) do
    //for scoring in Seq.initInfinite (fun _ -> getRandomInRanges ranges) |> Seq.map normalizeScoring do
    for scoring in Seq.initInfinite (fun _ -> getRandomNear [|-0.359;0.431;0.041;0.062;0.108|]) |> Seq.map normalizeScoring do
    //for scoring in [0.00001..0.01..0.2] |> Seq.map (fun w -> Array.append scoring' [| w |]) do
    //for param in [-20..1] |> List.rev do
        let bots =        
            (fun (game, player) -> new ScoreBot(game, player, scoring) :> Bot) :: (List.replicate (numPlayers - 1) (RandomBot >> botcast))
            //(fun (game, player) -> new DiversityBot(game, player, param) :> Bot) :: (List.replicate (numPlayers - 1) (RandomBot >> botcast))
            |> Seq.toArray       

        try
            let gameResults = runBots bots numGames

            let stats =
                gameResults
                |> Seq.groupBy (fun gameResult -> gameResult.Winner)
                |> Seq.sortBy fst
                |> Seq.map (fun (winner, gameResults) ->           
                    let winRate = (gameResults |> Seq.length |> float) / (float numGames)
                    let averagePoints = gameResults |> Seq.map (fun gameResult -> gameResult.Score) |> Seq.map float |> Seq.average
                    let averageGameLength = gameResults |> Seq.averageBy (fun gameResult -> float gameResult.GameLength)
                    winner, winRate, averagePoints, averageGameLength)

            let _, winRate, averagePoints, averageGameLength =
                stats
                |> Seq.tryFind (fun (winner, _, _, _) -> winner = 0)
                |? (0, 0.0, 0.0, 0.0)
        
            let averagePointsPerGame = winRate * averagePoints
            printfn "%s  %.3f %.3f %.2f" (printScoring scoring) winRate averagePointsPerGame averageGameLength
            //printfn "%d  %.3f %.3f %.2f" param winRate averagePointsPerGame averageGameLength
        with
        | e -> () //printfn "error"

[<EntryPoint>]
let main argv = 
    singleRun()
    //scoringEvaluation()
    0
