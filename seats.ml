open Player

type seats = player list

let add_player player seats = player :: seats

let size seats = List.length seats
