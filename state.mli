type bet = {
  bet_player: int;
  bet_amount: int;
  bet_paid_amt: (int*int) list;
}

type t = {
  game_type: int;
  num_players: int;
  table: Table.table;
  player_turn: int;
  button : int;
  players_in: int list;
  bet: bet;
  avail_action: string list;
  is_new_round : bool;
}
(** [game_type st] is the type of the game being played in [st].
    Requires: valid state [st]. *)
val game_type : t -> int

(** [num_players st] is the number of players in the game being played in [st].
    Requires: valid state [st]. *)
val num_players : t -> int

(** [table st] is the information about the table
    in the game being played in [st].
    Requires: valid state [st]. *)
val table : t -> Table.table

(** [player_turn st] is the type of the game being played in [st].
    Requires: valid state [st]. *)
val player_turn : t -> int

(** [players_in st] is the list of players who are playing
    in the current round
    of the game being played in [st].
    Requires: valid state [st]. *)
val players_in : t -> int list

(** [button st] is the player who is the button
    in the game being played in [st].
    Requires: valid state [st]. *)
val button : t -> int

(** [bet st] is the amount currently being bet
    in the game being played in [st].
    Requires: valid state [st]. *)
val bet : t -> bet

(** [button st] is the list of available actions
    in the game being played in [st].
    Requires: valid state [st]. *)
val avail_action : t -> string list

(** [button st] is true if a round has just started
    in the game being played in [st].
    Requires: valid state [st]. *)
val is_new_round : t -> bool

(** [button st] is the initial state
    of the game being played in [st].
    Requires: valid state [st]. *)
val init_state : int -> int -> int -> int -> t

val hand_order : int -> int -> int list

val bet_paid_amt : t -> (int*int) list

type move_result =
  | Legal of t
  | Illegal

val check : t -> move_result

val call : t -> move_result

val fold : t -> move_result

val stack : t -> move_result

val command_to_function : Command.command -> (t -> move_result)

(** [winner st] is the player that wins the round
    Requires that state has a nonempty list of players
    Requries there are 5 hole cards
    throws "cannot determine winner" exception if called on 
    list of empty players or hole cards less than 5*)
val winner : t -> Player.player

val get_avail_action : t -> t