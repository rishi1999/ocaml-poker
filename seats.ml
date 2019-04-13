(*Keeps track of players, dealers, and blinds*)
open Player

type seats = player list

let add_player (p:player) (s:seats) = p::s

let size (s:seats) = List.length s

