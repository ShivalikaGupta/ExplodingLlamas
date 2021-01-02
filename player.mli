open Deck

(** player is an abstract type represent the type of player *)
type player 

(** the empty player *)
val empty_player : player 

(** takes in a player and increases their point count by 2 *)
val add_points : player -> unit

(** takes in a player and returns the number of points they have *)
val get_points : player -> int

(** takes in a player and deducts their point count by 1 *)
val deduct_points : player -> unit

(** takes in a string for [player name], a player option for [next player],
    and a boolean for [ai status] where true indicates is ai and false 
    indicates is not ai *)
val new_player : string -> player option -> bool -> player

(** takes in a player and returns the player name *)
val get_name : player -> string

(** takes in a player and returns an int indicating the number of turns they 
    have to take, where 0 indicates it's not the player's turn, and all other
    positive integers indicates the number of turns *)
val get_turn : player -> int

(** takes in a player and integer which updates the number of turns the 
    player has to take *)
val update_turn : player -> int -> unit

(** takes in a player and returns the status, where true indicates that they're
    still alive and in the game, and false indicates that they're dead and not 
    playing anymore *)
val get_status : player -> bool

(** takes in a player and returns a bool, where true indicates that they're
    an ai, and false indicates that they're not an ai *)
val get_ai_status : player -> bool

(** takes in a player and a bool, which updates the status of the player *)
val update_status : player -> bool -> unit

(** takes in a player, and returns the hand of cards *)
val get_hand : player -> card list

(** takes in a player and a card, and updates the hand to contain the 
    additional card *)
val add_to_hand : player -> card -> unit

(** takes in a player and a card, and updates the hand to remove the 
    additional card. If the card does not exist within the player's hand, 
    then no cards will be removed and the player's hand will remain the same *)
val remove_from_hand : player -> card -> unit

(** takes in a player and returns the next player *)
val get_np : player -> player

(** takes in 2 players and updates the first player's [next player] field to
    be the second player *)
val update_np : player -> player -> unit
