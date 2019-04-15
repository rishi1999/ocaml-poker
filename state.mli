type t
type bet

val game_type : t -> int

val num_players : t -> int

val table : t -> Table.table

val player_turn : t -> int

val players_in : t -> int list

val button : t -> int

val bet : t -> bet

val avail_action : t -> string list

val is_new_round : t -> bool

val init_state : int -> int -> int -> int -> t

val hand_order : int -> int -> int list

type check_result =
  | Legal of t
  | Illegal

val check : t -> check_result
