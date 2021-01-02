open Gamestate
open Deck
open Player

(** [shuffle g] is the same as Gamestae.deck, but all elements are reordered
    randomly*)
val shuffle : game -> unit

(** [Prophet_llama g] Rearranges the deck from g using 2 inputs from the 
    player who played it (the current player in g) *)
val prophet_llama : game -> unit

(** [rank_card card] takes a card and returns the value of the card,
    which corrresponds to how good the card is, higher is better*)
val rank_card : card -> int

(** [compare_card_use card1 card2] compares card1 with card2. If card1
    has a better rank than card2, [compare_card_use] returns 1, if card2 has
    a better rank than card1, [compare_card_use] returns -1, otherwise 0 *)
val compare_card_use : card -> card -> int

(** [skip player] does a pass action, without the drawing requirement *)
val skip : game -> unit

(** [attack player] forces the player to take two turns *)
val attack: game -> unit

(** [favor player1 player2] forces player2 to give a card to player 1*)
val favor : game -> unit

(** [check_turn g current_player] checks if the current_player was attacked
    and forces said player to take another turn if the player has been 
    attacked, otherwise it will be the next player's turn *)
val check_turn : game -> player -> unit

(** [defuse game] negates the losing condition for current player who drew
    the KABLLAMA! card *)
val defuse : game -> unit

(** [insert_card_at card_lst card i] inserts [card] at position [i] in
    [card_lst] *)
val insert_card_at : Deck.card list -> Deck.card -> int -> Deck.card list


(** [use_card card] activates the card and mutates something, returns unit *)
val use_card : Deck.card -> game -> unit