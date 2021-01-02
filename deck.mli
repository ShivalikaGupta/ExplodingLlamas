(** 
   Representation of dynamic deck data.

   This module represents the data stored in deck, including
   the card remaining and the cards used. 
*)

(** card_prop is the property of each card, including name and description *)
type card_prop = {name : string; description: string}

(** The type of cards there are in this game *)
type card = 
    Skip of card_prop            (* Skips next persons turn *)
  | Attack of card_prop             (* Next person takes two turns *)
  | Shuffle of card_prop          (* Shuffles remaining cards *)
  | Nope of card_prop             (* Negates the previous card played *)
  | ProphetLlama of card_prop      (* Rearranges Top three cards *)
  | Favor of card_prop               (* Asks a player for a card *)
  | Defuse of card_prop             (* Stops Exploding Llama *)
  | Kabllama of card_prop        (* The Exploding card *)
  | SurfingLlama of card_prop      (* Normal card *) 
  | EmoLlama of card_prop          (* Normal card *)
  | CarrotLlama of card_prop        (* Normal card *)
  | HairyLlama of card_prop         (* Normal card *)
  | CodingLlama of card_prop      (* Normal card *)

(** [deck] is a composition of all the card in the deck, in total, there
    are 3 Kabllama, 2 Defuse (4 Defuses are already in the starting hands
    of 4 players), 4 Skip, 4 Shuffle, 4 Attack, 4 Favor, 5 Nope,
    5 ProphetLlama, 4 SurfingLlama, 4 EmoLlama, 4 CarrotLlama, 4 CodingLlama,
    4 HairyLlama*)
val deck : card list

(** type t is the representation of the deck *)
type t =  card list

(** [next_card t] is the next card from t*)
val next_card : t -> card option

(** [shuffle d] takes [d] and reorders the elements in d, but nothing
    else is changed  *)
val shuffle : t -> t

(** [get_card_name card] returns the name of [card] from its properties *)
val get_card_name : card -> string

(** [get_card_name card] returns the description of [card] from its properties
*)
val get_card_description : card -> string
