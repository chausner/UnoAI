module Bot

open Card
open Game

type BotAction =
    | PlayCardBotAction of Card
    | DrawCardBotAction of (Card -> Card option)

[<AbstractClass>]
type Bot() =
    abstract member Initialize: State -> unit
    abstract member OnActionPerformed: State -> Action -> State -> unit
    abstract member PerformAction: unit -> BotAction

    default self.Initialize state = ()
    default self.OnActionPerformed prevState action state = ()