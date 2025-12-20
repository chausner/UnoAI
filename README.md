# UnoAI
Implementation of the card game rules of [UNO](https://en.wikipedia.org/wiki/Uno_(card_game)) in F# to evaluate different playing strategies

## Features
* Implementation of the core game logic of UNO
* Eight different bot implementations
* Multi-threaded batch simulation to determine the average win rate and game points for each bot over a large number of games

## Bots
The following bots are currently implemented, each following a different playing strategy.
Some of the bots have parameters which can be used to optimize its behavior towards a high win rate or a high number of points.

### RandomBot
Bot that makes all decisions in a random manner.

* Out of all playable cards, a random one is selected.
* Only if none of the cards in the bot's hand can be played, a card is drawn.
* When a drawn card can be played, it is always played.
* When a Wild or Wild Draw Four is played, the color is chosen randomly.

For more details, see the implementation in [RandomBot.fs](UnoAI/Bots/RandomBot.fs).

### GreedyBot
Bot that makes decisions in a greedy manner (i.e. chooses the option that gives the highest immediate benefit).

* Out of all playable cards, the card with the highest score (the number of points associated with a card, according to the game rules) is chosen.
  If more than one card has the highest score, a random one of them is chosen.
* Only if none of the cards in the bot's hand can be played, a card is drawn.
* When a drawn card can be played, it is always played.
* When a Wild or Wild Draw Four is played, the color that is most common in the player's hand is chosen.
  If more than one color is most common, a random one of them is chosen.

The score of a card is determined as follows:

| Card           | Score                  |
|----------------|-----------------------:|
| Standard Card  | \<number on the card\> |
| Skip           |                     20 |
| Draw Two       |                     20 |
| Reverse        |                     20 |
| Wild           |                     50 |
| Wild Draw Four |                     50 |

For more details, see the implementation in [GreedyBot.fs](UnoAI/Bots/GreedyBot.fs).

### DiversityBot
Bot that chooses cards in such a way that the diversity of cards in the bot's hand is kept maximized.

* Out of all playable cards, the card that matches with the highest number of other cards in hand (not counting Wild and Wild Draw Four cards) is played.
  If more than one card matches with the highest number of other cards, a random one of them is chosen.
  Wild and Wild Draw Four are only played if no other card matches.
* Only if none of the cards in the bot's hand can be played, a card is drawn.
* When a drawn card can be played, it is always played.
* When a Wild or Wild Draw Four is played, the color that is most common in the player's hand is chosen.
  If more than one color is most common, a random one of them is chosen.

For more details, see the implementation in [DiversityBot.fs](UnoAI/Bots/DiversityBot.fs).

### CardRankingBot
Bot that chooses cards based on a fixed ranking.

* Out of all playable cards, the card that has the highest rank is chosen.
  If more than one card has the highest rank, a random one of them is chosen.
* Only if none of the cards in the bot's hand can be played, a card is drawn.
* When a drawn card can be played, it is always played.
* When a Wild or Wild Draw Four is played, the color that is most common in the player's hand is chosen.
  If more than one color is most common, a random one of them is chosen.

The default ranking when optimizing for win rate is:

Draw Two > Skip > Standard Card > Reverse > Wild Draw Four > Wild

The default ranking when optimizing for game points is:

Skip > Standard Card > Draw Two > Reverse > Wild > Wild Draw Four

For more details, see the implementation in [CardRankingBot.fs](UnoAI/Bots/CardRankingBot.fs).

### MixBot
Bot that combines the logic of CardRankingBot and DiversityBot with a couple of extra rules.

* Certain types of cards are only played if the number of cards of other types in the bot's hand is less than or equal to a certain limit.
* Cards of the color(s) that are most common in the bot's hand are preferred over cards of other colors.
* Cards are first ranked according to the logic of CardRankingBot.
  If multiple candidates have the same highest rank, they are further ranked according to the logic of DiversityBot.
* If the next player has only one card left, different card ranking rules are applied that favor Draw Two, Wild Draw Four, Skip and Reverse.
* Certain types of cards are only played after being drawn if the number of cards in the bot's hand is less than or equal to a certain limit.

The default parameters when optimizing for win rate are:

* Wild cards are only ever played if the number of cards in the bot's hand that are neither Wild nor Wild Draw Four is less than or equal to 2.
* Wild Draw Four cards are only ever played if the number of cards in the bot's hand that are neither Wild nor Wild Draw Four is less than or equal to 4.
* Wild and Wild Draw Four cards are only played after being drawn if the number of cards in the bot's hand is less than or equal to 2.

The default parameters when optimizing for game points are:

* Wild cards are only ever played if the number of cards in the bot's hand that are neither Wild nor Wild Draw Four is less than or equal to 5.
* Wild Draw Four cards are only ever played if the number of cards in the bot's hand that are neither Wild nor Wild Draw Four is less than or equal to 3.
* Wild and Wild Draw Four cards are only played after being drawn if the number of cards in the bot's hand is less than or equal to 2.

For more details, see the implementation in [MixBot.fs](UnoAI/Bots/MixBot.fs).

### MixBot2
Bot that extends the logic of CardRankingBot with a couple of extra rules.

* Cards are first ranked according to the logic of CardRankingBot (and using separate end-game ranks when the next player has only one card left).
* Cards of the color(s) that are most common in the bot's hand are preferred over cards of other colors.
* When playing a Wild or Wild Draw Four, colors that are common in the bot's hand are preferred (with a configurable slack).
* Certain types of cards are only played if the number of cards in the bot's hand is less than or equal to a certain limit
  (by default this is used to hold Wild and Wild Draw Four cards until few non-Wild cards remain), with separate limits in end-game mode.
* Card counting information is used as a final tie-breaker to prefer cards that the next active player is least likely to be able to match.
* Certain types of cards are only played after being drawn if the number of cards in the bot's hand is less than or equal to a certain limit.

The default parameters when optimizing for win rate are:

* Card ranking: Draw Two > Skip > Standard Card > Reverse > Wild Draw Four > Wild
* End-game card ranking: Draw Two > Wild Draw Four > Skip > Reverse > Standard Card > Wild
* Wild cards are only ever played if the number of non-Wild cards in the bot's hand is less than or equal to 2.
* Wild Draw Four cards are only ever played if the number of non-Wild cards in the bot's hand is less than or equal to 4.
* End-game card count limits are disabled.
* When playing a Wild or Wild Draw Four, any of the most common colors within 1 card of the maximum are considered preferred options.
* Wild and Wild Draw Four cards are only played after being drawn if the number of cards in the bot's hand is less than or equal to 2.

The default parameters when optimizing for game points are:

* Card ranking: Skip > Standard Card > Draw Two > Reverse > Wild > Wild Draw Four
* End-game card ranking: Draw Two > Wild Draw Four > Skip > Reverse > Standard Card > Wild
* Wild cards are only ever played if the number of non-Wild cards in the bot's hand is less than or equal to 5.
* Wild Draw Four cards are only ever played if the number of non-Wild cards in the bot's hand is less than or equal to 3.
* End-game card count limits are disabled.
* When playing a Wild or Wild Draw Four, any of the most common colors within 2 cards of the maximum are considered preferred options.
* Wild and Wild Draw Four cards are only played after being drawn if the number of cards in the bot's hand is less than or equal to 2.

For more details, see the implementation in [MixBot2.fs](UnoAI/Bots/MixBot2.fs).

### ScoreBot
Bot that chooses cards in such a way to maximize a scoring function defined on the cards in the bot's hand.

* The scoring function is defined as the sum of card type-specific scores over the cards in the bot's hand.
* The scoring function is also used to decide which color to choose when a Wild or Wild Draw Four is placed.
* The scoring function is also used to decide whether a drawn card should be played or not.

The default card type-specific scores when optimizing for win rate are:

| Card           | Score  |
|----------------|-------:|
| Standard Card  | -0.168 |
| Reverse        | -0.163 |
| Skip           | -0.190 |
| Draw Two       | -0.227 |
| Wild           |  0.048 |
| Wild Draw Four | -0.035 |

The default card type-specific scores when optimizing for game points are:

| Card           | Score  |
|----------------|-------:|
| Standard Card  | -0.189 |
| Reverse        | -0.153 |
| Skip           | -0.206 |
| Draw Two       | -0.193 |
| Wild           |  0.015 |
| Wild Draw Four |  0.023 |

For more details, see the implementation in [ScoreBot.fs](UnoAI/Bots/ScoreBot.fs).

### CardCountingBot
Bot that uses a technique similar to [card counting](https://en.wikipedia.org/wiki/Card_counting) to keep track of who played which cards over time,
with the goal of estimating the probabilities that other players have certain types of cards.
The idea is to ultimately choose cards that likely cannot be matched by the next player.

This bot is currently in an experimental state and should be considered work-in-progress.

For more details, see the implementation in [CardCountingBot.fs](UnoAI/Bots/CardCountingBot.fs).

## Evaluation
The following statistics have been determined over a total of 10,000,000 simulated games.

### 2 players, * vs. RandomBot

| Bot                    | Win Rate (%) | Average Points |
| -----------------------| ------------:| --------------:|
| RandomBot              |         50.0 |           20.7 |
| GreedyBot              |         55.4 |           23.6 |
| DiversityBot           |         57.5 |           29.7 |
| CardRankingBot (\*)    |         63.4 |           30.8 |
| CardRankingBot (\*\*)  |         59.5 |           33.4 |
| MixBot (\*)            |         66.4 |           33.2 |
| MixBot (\*\*)          |         63.6 |           37.4 |
| MixBot2 (\*)           |     **68.8** |           33.8 |
| MixBot2 (\*\*)         |         66.6 |       **38.8** |
| ScoreBot (\*)          |         55.8 |           22.8 |
| ScoreBot (\*\*)        |         55.5 |           23.8 |
| CardCountingBot (\*)   |         57.5 |           23.1 |
| CardCountingBot (\*\*) |         57.4 |           23.9 |

(\*) parameters optimized on win rate, (\*\*) parameters optimized on game points

### 6 players, * vs. 5 RandomBots

| Bot                    | Win Rate (%) | Average Points |
| -----------------------| ------------:| --------------:|
| RandomBot              |         16.7 |           31.4 |
| GreedyBot              |         15.8 |           29.4 |
| DiversityBot           |         22.9 |           45.5 |
| CardRankingBot (\*)    |         21.7 |           42.7 |
| CardRankingBot (\*\*)  |         21.4 |           43.3 |
| MixBot (\*)            |         23.6 |           46.2 |
| MixBot (\*\*)          |         23.5 |           47.4 |
| MixBot2 (\*)           |     **24.2** |           47.4 |
| MixBot2 (\*\*)         |         24.1 |       **48.8** |
| ScoreBot (\*)          |         20.5 |           39.6 |
| ScoreBot (\*\*)        |         20.5 |           40.0 |
| CardCountingBot (\*)   |         22.4 |           43.5 |
| CardCountingBot (\*\*) |         22.4 |           43.9 |

(\*) parameters optimized on win rate, (\*\*) parameters optimized on game points

## License
MIT, see [LICENSE](LICENSE)
