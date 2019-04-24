open Deck
open Player

(** [table] represents a poker table described using the following
    information:
    [pot] : money accumulated from bets and blinds
    [blind] : integer id of player who is blind for the current round
    [participants] : list of players in the game
    [board] : cards on the board *)
type table = {
  pot: int;
  blind: int;
  participants: Player.player list;
  board: Deck.card list;
}

(** [pot tab] returns the total current pot of the table [tab]
    Requires: valid table tab
    Example: [pot {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = [(Spade, Ace)]}] = 0 *)
val pot : table -> int

(** [blind tab] returns the blind player number of the table [tab]
    Requires [tab] is a valid table
    Example : [blind {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = [(Spade, Ace)]}] = 1 *)
val blind : table -> int

(** [participants tab] returns the current total player list of table [tab]
    Requires [tab] is a valid table
    Example: [participants {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = [(Spade, Ace)]}] = 
    [{id = 1; cards = []; money = 20}] *)
val participants : table -> Player.player list

(** [board tab] returns the board card list of table [tab]
    Requires [tab] is a valid table
    Example : [board {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = [(Spade, Ace)]}] = [(Spade, Ace)] *)
val board : table -> (Deck.card) list

(** [nth_participant tb n] gets the number [n] player in the table [tb]. 
    It is 0-based, so the first player in the list is retrieved using n = 0.
    Requires : [tb] is a valid table,
    n is a valid index for the participants list. 
    Example: [nth_participant table 2] is the third player in the game. *)
val nth_participant : table -> int -> Player.player

(** [next_round_players tab] returns the table [tab] with blind
     updated for the next round
    Requires [tab] is a valid table
    Example: [next_round_players {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20};
                    {id = 2; cards = []; money = 30}];
    board = [(Spade, Ace)]}] = 
    {pot = 0; blind = 2;
    participants = [{id = 1; cards = []; money = 20};
                    {id = 2; cards = []; money = 30}];
    board = [(Spade, Ace)]} *)
val next_round_players: table -> table

(** [deal tab] deals 2 cards to each player in [tab] and returns it
    Requires: [tab] is a valid table
    Requires: each participant in [tab] has 0 cards
    Raises: "player has non 0 cards" exception if any player doesn't have 
    0 cards
    Example: [deal {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = [(Spade, Ace)]}] = 
    [{id = 1; cards = []; money = 20}] = 
    {pot = 0; blind = 1;
    participants = [{id = 1; cards = [(Spades, Ace);(Clubs, Ace)]; money = 20};
                {id = 2; cards = [(Spades, Two);(Hearts, Three)]; money = 30}];
    board = [(Spade, Ace)]} *)
val deal: table -> table

(** [add_to_hole tab] adds cards to the board depending on
    how many are already there
    Requires: [tab] is a valid table
    Example: [add_to_board {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = []}] = 
    [{id = 1; cards = []; money = 20}] =
    {pot = 0; blind = 1;
    participants = [{id = 1; cards = []; money = 20}];
    board = [(Spade, Ace);(Spades, Two);(Hearts, Three)]} = *)
val add_to_board:  table -> table

(** [clear_players p] clears the cards of each player in player list [p]
    Requires [p] is a valid player list
    Example: [clear_players {pot = 0; blind = 1;
    participants = [{id= 1; cards= [(Spades, Ace);(Clubs, Ace)]; money = 20}];
    board = []}] =
    {pot = 0; blind = 1;
    participants = [{id= 1; cards= []; money = 20}];
    board = []} *)
val clear_players: player list -> player list -> player list

(** [clear_round table x] is the table [x] with the cards cleared.
    Requires [x] is a valid table
    Example: [clear_round {pot = 0; blind = 1;
    participants = [{id= 1; cards= []; money = 20}];
    board = [(Spades, Ace)]}] =
    {pot = 0; blind = 1;
    participants = [{id= 1; cards= []; money = 20}];
    board = []} *)
val clear_round: table -> table
