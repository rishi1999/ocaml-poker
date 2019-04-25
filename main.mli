(** [play_game f] plays the game with the file with name [f].
    The user inputs commands to control the game.
    Specifications of these commands are in [command.mli].
    Requires: [f] is a text file that contains game state.
    If the data is in an invalid format,
    or if [f] does not exist, an exception will be thrown.
    Example: [play_game poker.txt] plays the game with [poker.txt]. *)
val play_game : State.t -> unit

(*Note to Grader: Please maximize the terminal window before
  running make play in order to have the best experience and
  avoid any issues with the UI not fully displaying. We designed
  this for a native OCaml installation on a 15 inch laptop
  screen running a maximized terminal. If you use a smaller terminal,
  the ASCII art may not display correctly and you have to scroll to
  see the full UI for each new turn in the game. *)