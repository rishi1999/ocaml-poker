open Player
open ANSITerminal

let win_descrp = [|
  "Winstreak broken.";
  "You are gaining popularity";
  "You are kind of a big deal";
  "Unstoppable";
  "You are a nightmare";
  "If this was a real casino, you would be kicked out";
|]

let dyndescrp player = let index = player.consecutive_wins in
  let style = match index with
    | 1 -> blue
    | 2 -> green
    | 3 -> yellow
    | 4 -> red
    | 5 -> magenta
    | _ -> black in
  ANSITerminal.(print_string [style; Background White] win_descrp.(index))
