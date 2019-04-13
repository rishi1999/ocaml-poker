type t
type bet 

val game_type : t -> int

val player_number : t -> int

val table : t -> Table.table

val player_turn : t -> int

val players_in : t -> int list

val button : t -> int

val bet : t -> bet

val avail_action : t -> string list

val init_state : int -> int -> int -> int -> t