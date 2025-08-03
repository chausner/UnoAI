module BotUtils

open Card
open Utils

let getRandomColor () =
    [| Red; Green; Blue; Yellow |] |> Array.chooseRandom

let chooseColorIfNeeded card color =
    match card with
    | Wild None
    | WildDrawFour None -> chooseColor card (color ())
    | _ -> card