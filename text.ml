(* *)
open Deck


let spades = ["┌─────────┐"]

let card_printer cardlist =
  let element = match card with
    | (Diamonds, rank) -> diamonds.(rank - 1)
    | (Hearts, rank) -> hearts.(rank - 1)
    | (Spades, rank) -> spades.(rank - 1)
    | (Clubs, rank) -> clubs.(rank - 1) in
  let string_list = List.map element cardlist in
  let rec printer card_list = match card_list with
    | [] -> ()
    | h :: t -> print_endline h;
      let current_cursor = ANSITerminal.pos_cursor () in
      ANSITerminal.move_cursor (fst current_cursor - 10) (snd current_cursor + 3) in
  printer string_list