(** [card_printer cardlist] prints the ASCII representation of each card
    in cardlist horizontally across the screen.
    Requires: cardlist contains valid tuples of type Deck.suit * Deck.rank
    Example: [card_printer [Diamonds, Ace]]
    is 
     ┌─────────┐    
     │A        │    
     │         │    
     │         │    
     │    ♦    │    
     │         │    
     │         │    
     │        A│
     └─────────┘ *)

val card_printer : Deck.card list -> unit