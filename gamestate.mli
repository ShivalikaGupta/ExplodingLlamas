open Deck 
open Player

(** The representation of type game.
    Game should take first player, allow them to play any number of cards, 
    then draw, move onto next player and repeat *)
type game 

exception Empty

(** The starting state of the game, where there are 4 players, and each player
    starts with a hand of 5 cards *)
val startstate : game

(** The empty game *)
val emptygame : game 

(** Takes in a gamestate and updates [current player] to next player *)
val update_next_player : game -> unit

(** Takes in a gamestate and a player of the gamestate, and updates 
    [current player] to the player *)
val update_current_player : game -> player -> unit

(** Takes in a gamestate, and deals 5 cards (4 random, 1 defuse) to each 
    player's hands *)
val starting_deal : game -> unit

(** Takes in a gamestate and adds the top card of the deck to the current 
    player's hand *)
val draw : game -> unit

(** Takes in a gamestate and a card and adds the card to the gamestate's 
    discard pile *)
val discard : game -> card -> unit 

(** Takes in a gamestate and returns the [player list] *)
val get_player_list : game -> player list

(** Takes in a gamestate and returns the [current player] *)
val get_current_player : game -> player

(** Takes in a gamestate and returns the [deck] *)
val get_deck : game -> card list

(** Takes in a gamestate and a deck (card list) and updates the gamestate's 
    [deck] to the deck passed in *)
val update_deck: game -> card list -> unit
