module BotUtils

open Card
open Utils

let getRandomColor () =
    allCardColors |> Array.chooseRandom

let chooseColorIfNeeded card color =
    match card with
    | Wild None
    | WildDrawFour None -> chooseColor card (color ())
    | _ -> card