open Deck
open Player

type table = {
  dealer: int;
  blind: int;
  participants: Player.player list;
  hole_cards: (Deck.suit * Deck.rank) list;
}

let dealer tb = tb.dealer
let blind tb = tb.blind
let participants tb = tb.participants
let hole_cards tb = tb.hole_cards

let next_round_players (tab:table) = match tab with
  |
    {
      dealer = d;
      blind;
      participants;
      hole_cards;
    } as tab when blind = List.length participants + 1
    ->
    {
      tab with
      dealer = d + 1; blind = 1;
    }
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab when dealer = List.length participants + 1
    ->
    {
      tab with
      dealer = 1;
      blind = 2;
    }
  | {
    dealer = d;
    blind = b;
    participants;
    hole_cards;
  } as tab
    ->
    {
      tab with
      dealer = d+1;
      blind = b+1;
    }

let deal (table : table) : table =
  Deck.deck_init;
  Deck.shuffle_deck;
  let deal_helper (player:player) = match player with
    | {
      id;
      cards = c;
      money;
    } when List.length c <> 0 -> failwith "player issue"
    | pl ->
      {
        pl with
        cards = Deck.pick_cards 2;
      }
  in
  let rec deal_to_each players list=
    match players with
    | [] -> list
    | player :: t -> deal_to_each t ((deal_helper player) :: list)

  in
  match table with
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab
    ->
    {
      tab with
      participants = deal_to_each participants [];
    }

let add_to_hole = function
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } when List.length hole_cards > 5
    -> failwith "too many hole cards"
  |
    {
      dealer;
      blind;
      participants;
      hole_cards = h;
    } when List.length h = 0
    ->
    {
      dealer;
      blind;
      participants;
      hole_cards = Deck.pick_card :: Deck.pick_card :: Deck.pick_card :: h;
    }
  | 
    {
      dealer;
      blind;
      participants;
      hole_cards = h;
    }
    ->
    {
      dealer;
      blind;
      participants;
      hole_cards = Deck.pick_card :: h;
    }


let rec clear_players (p:player list) list = match p with
  | [] -> list
  | pl :: t
    -> clear_players t
         (
           {
             pl with
             cards = [];
           } :: list
         )

(** [clear_round table x] is the table with the cards cleared. *)
let rec clear_round table = match table with
  |
    {
      dealer;
      blind;
      participants;
      hole_cards;
    } as tab
    ->
    {
      tab with
      participants = clear_players participants [];
      hole_cards = [];
    }
