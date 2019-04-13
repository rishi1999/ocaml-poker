(** [play_game f] plays the game with the file with name [f].
    The user inputs commands to control the game.
    Specifications of these commands are in [command.mli].
    Requires: [f] is a text file that contains game state.
    If the data is in an invalid format,
    or if [f] does not exist, an exception will be thrown.
    Example: [play_game poker.txt] plays the game with [poker.txt].
*)
(* val init_game : int -> unit *)