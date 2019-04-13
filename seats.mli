(*Keeps track of players, dealers, and blinds*)
open Player

type seats = player list

val add_player: player -> seats -> seats

val size: seats -> int

