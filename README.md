# UnoAI
Implementation of the card game rules of [UNO](https://en.wikipedia.org/wiki/Uno_(card_game)) in F# to evaluate different playing strategies

## Features
* Implementation of the core game logic of UNO
* Seven different bot implementations
* Multi-threaded batch simulation to determine the average win rate and game points for each bot over a large number of games

## Bots
The following bots are currently implemented, each following a different playing strategy:

* [`RandomBot`](UnoAI/Bots/RandomBot.fs): makes decisions completely randomly (e.g. out of all playable cards, plays a random one)
* [`GreedyBot`](UnoAI/Bots/GreedyBot.fs): makes decisions in a greedy manner (i.e. chooses the option that gives the highest immediate benefit)
* [`DiversityBot`](UnoAI/Bots/DiversityBot.fs): chooses cards in a way to keep the diversity of cards in the bot's hand high
* [`CardRankingBot`](UnoAI/Bots/CardRankingBot.fs): chooses cards based on a fixed ranking
* [`MixBot`](UnoAI/Bots/MixBot.fs): combines the logic of `DiversityBot` and `CardRankingBot` with some extra tweaks
* [`ScoreBot`](UnoAI/Bots/ScoreBot.fs): chooses cards to maximize a scoring function defined on the cards in the bot's hand
* [`CardCountingBot`](UnoAI/Bots/CardCountingBot.fs): uses card counting to choose cards that likely cannot be matched by the next player

Some of the bots have parameters which can be used to optimize for win rate or number of points.
For more details on the playing strategy of each bot, see the implementations in source code.

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
| MixBot (\*)            |     **64.4** |           22.2 |
| MixBot (\*\*)          |         59.2 |       **36.7** |
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
| MixBot (\*)            |     **23.4** |           46.1 |
| MixBot (\*\*)          |         23.2 |       **47.2** |
| ScoreBot (\*)          |         20.5 |           39.6 |
| ScoreBot (\*\*)        |         20.5 |           40.0 |
| CardCountingBot (\*)   |         22.4 |           43.5 |
| CardCountingBot (\*\*) |         22.4 |           43.9 |

(\*) parameters optimized on win rate, (\*\*) parameters optimized on game points

## License
MIT, see [LICENSE](LICENSE)
