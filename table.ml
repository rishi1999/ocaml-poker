(*Keeps track of players, dealers, and blinds*)


type player = string
type players = player list
type table = {dealer: int; blind: int; participants: players; hole_cards: int list}



let next_round_players table = function
  |{dealer = d; blind = b; participants = p; hole_cards = c} when b = List.length(p)+1 
    -> { dealer = (d + 1); blind = b ; participants = p; hole_cards = c}
  |{dealer = d; blind = b; participants = p; hole_cards = c} when b = List.length(p)+1
    -> {dealer = 1; blind = 2 ; participants = p; hole_cards = c}
  |{dealer = d; blind = b; participants = p; hole_cards = c}
    -> {dealer = d; blind = b; participants = p; hole_cards = c}

