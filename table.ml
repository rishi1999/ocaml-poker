(*Keeps track of players, dealers, and blinds*)
open Deck

type player = {id: int; cards: (Deck.suit * Deck.rank) list; money: int}
type table = {dealer: int; blind: int; participants: player list; 
hole_cards: (Deck.suit * Deck.rank) list}



let next_round_players table = function
  |{dealer = d; blind = b; participants = p; hole_cards = c} when b = List.length(p)+1 
    -> { dealer = (d + 1); blind = b ; participants = p; hole_cards = c}
  |{dealer = d; blind = b; participants = p; hole_cards = c} when b = List.length(p)+1
    -> {dealer = 1; blind = 2 ; participants = p; hole_cards = c}
  |{dealer = d; blind = b; participants = p; hole_cards = c}
    -> {dealer = d; blind = b; participants = p; hole_cards = c}


let deal (table:table)=
  Deck.deck_init;
  Deck.shuffle_deck;
  let deal_helper = function
    | {id = s; cards = []; money = m}
      -> {id = s; cards = Deck.pick_cards 2; money = m}
    | _ -> failwith "player issue"
  in
  let rec deal_to_each players list=
    match players with
    | [] -> list
    | player::t -> deal_to_each t ((deal_helper player)::list)

  in
  match table with
  |{dealer = d; blind = b; participants = p; hole_cards = c} 
    -> {dealer = d; blind = b; participants = (deal_to_each p []); hole_cards = c}

let add_to_hole (table:table) = function
  |{dealer = d; blind = b; participants = p; hole_cards = c} when List.length c > 3
    -> failwith "too many hole cards"
  |{dealer = d; blind = b; participants = p; hole_cards = c}
    -> {dealer = d; blind = b; participants = p; hole_cards = (Deck.pick_card::c)}


let rec clear_players (p:player list) list= match p with
  |[] -> list
  |{id = s; cards = c; money = m}::t
    -> clear_players t ({id = s; cards = []; money = m}::list)

(** only clears cards from table*)
let rec clear_round table = function
  |{dealer = d; blind = b; participants = p; hole_cards = c}
    -> {dealer = d; blind = b; participants = (clear_players p []); hole_cards = []}

