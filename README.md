# OCaml Poker

Texas hold 'em game built in OCaml

## Modes

### Single-player

Users can play against 3 difficulty levels of AI:
* The easy bot is very predictable and risk averse.
* The medium bot uses a Monte Carlo simulation to calculate a near-optimal move given the current state game.
* The hard bot uses the same logic but runs significantly more simulations to generate more accurate results.

### Multi-player

Up to 10 players can play at a time. To prevent users from seeing other players' cards, players' hands are hidden by default and can be temporarily shown using the `show` command.
