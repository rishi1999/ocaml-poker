(** [bet] is the bet situation of the current round:
    [bet_player] : the player that has bet / raised the last
    [bet_amount] : the current bet amount that the next player has to match
    [bet_paid_amt] : the current bet situation in form (player, bet_amount) list
*)
type bet = {
  bet_player: int;
  bet_amount: int;
  bet_paid_amt: (int*int) list;
}

(** [t] is the state of the game described using the following information:
    [game_type] : an integer representin a game type
      0 if it is a multiplayer game, 1 if it is against the AI
    [num_players] : the number of players in the game
    [table] : type Table.table that represents the table
    [player_turn] : the player that has the action
    [button] : the person that goes last in the hand
    [players_in] : the list of players that are currently playing the hand
    [bet] : current bet situation in this round
    [avail_action] : the available actions that the current player can act
*)
type t = {
  game_type: int;
  num_players: int;
  table: Table.table;
  player_turn: int;
  button : int;
  players_in: int list;
  players_played: int list;
  bet: bet;
  avail_action: string list;
  winner: int;
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

val continue_game : t -> t

(** [winning_player st] is the player that has won the hand in state [st].
    Requires: valid state [st]. *)
val winning_player : t -> int

(** [bet st] is the amount currently being bet
    in the game being played in [st].
    Requires: valid state [st]. *)
val bet : t -> bet

(** [button st] is the list of available actions
    in the game being played in [st].
    Requires: valid state [st]. *)
val avail_action : t -> string list

(** [button st] is the initial state
    of the game being played in [st].
    Requires: valid state [st]. *)
val init_state : int -> int -> int -> int -> t

val hand_order : int -> int -> int list

(** [bet_paid_amt st] is the list of tuples of players
    and how much money they have paid in state [st]. *)
val bet_paid_amt : t -> (int * int) list

(** [move_result] is the type representing the result
    of a player executing a command. *)
type move_result =
  | Legal of t
  | Illegal of string

(** [check st] is the result of the player calling the check command.
    Requires: valid state [st]. *)
val check : t -> move_result

(** [call st] is the result of the player calling the call command.
    Requires: valid state [st]. *)
val call : t -> move_result

(** [fold st] is the result of the player calling the fold command.
    Requires: valid state [st]. *)
val fold : t -> move_result

(** [stack st] is the result of the player calling the stack command.
    Requires: valid state [st]. *)
val stack : t -> move_result

(** [command_to_function comm] is the function in State
    associated with the command [comm].
    Requires: valid command [comm]. *)
val command_to_function : Command.command -> (t -> move_result)

(** [winner st] is the player that wins the round
    Requires that state has a nonempty list of players
    Requries there are 5 hole cards
    throws "cannot determine winner" exception if called on
    list of empty players or hole cards less than 5*)
val winner : t -> Player.player

(** [get_avail_action st] is the list of valid commands
    the player can currently execute.
    Requires: valid state [st]. *)
val get_avail_action : t -> t