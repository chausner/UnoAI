# UnoAI
Implementation of the card game rules of UNO in F# to evaluate different playing strategies

## Features
* Compute win rate and average points the following playing strategies (bots)
  * `RandomBot`: makes decisions completely randomly (e.g. out of all playable cards, plays a random one)
  * `GreedyBot`: makes decisions in a greedy manner (i.e. chooses the option that gives the highest immediate benefit)
  * `DiversityBot`: chooses cards in a way to reduce the diversity of cards in the bot's hand
  * `CardRankingBot`: chooses cards based on a fixed ranking
  * `ScoreBot`: chooses cards to maximize a scoring function defined on the cards in the bot's hand
  * `CardCountingBot`: uses card counting to choose cards that likely cannot be matched by the next player

## License
MIT, see [LICENSE](LICENSE)
