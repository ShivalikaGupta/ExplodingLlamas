type card_prop = {name : string; description: string}

type card = 
    Skip of card_prop             (* Skips next persons turn *)
  | Attack of card_prop           (* Next person takes two turns *)
  | Shuffle of card_prop          (* Shuffles remaining cards *)
  | Nope of card_prop             (* Negates the previous card played *)
  | ProphetLlama of card_prop     (* Rearranges Top three cards *)
  | Favor of card_prop            (* Asks a player for a card *)
  | Defuse of card_prop           (* Stops Exploding Llama *)
  | Kabllama of card_prop         (* The Exploding card *)
  | SurfingLlama of card_prop     (* Normal card *) 
  | EmoLlama of card_prop         (* Normal card *)
  | CarrotLlama of card_prop      (* Normal card *)
  | HairyLlama of card_prop       (* Normal card *)
  | CodingLlama of card_prop      (* Normal card *)

type t = card list

let rec add_card card times deck = 
  if times = 0 
  then deck 
  else add_card card (times - 1) (card :: deck)

let build_deck () =
  add_card (Kabllama 
              {name = "Kabllama"; description = "BOOM U DIE"}) 3 [] |>
  add_card (Defuse 
              {name = "Defuse"; description = "FIX BOMBS"}) 2 |>
  add_card (Skip 
              {name = "Skip"; description = "End turn, don't draw"}) 4 |>
  add_card (Shuffle 
              {name = "Shuffle"; description = "Shuffle Deck"}) 4 |>
  add_card (Attack 
              {name = "Attack"; 
               description = "End Turn. Next Player takes two turns"}) 4 |>
  add_card (Favor 
              {name = "Favor"; 
               description = "Pick another player.
                              They give you any card they choose."}) 4 |>
  add_card (Nope 
              {name = "Nope";
               description = "Negate last card played, " ^ 
                              "can be played any time."}) 5 |>
  add_card (ProphetLlama 
              {name = "ProphetLlama"; 
               description = "Look at top 3 cards, 
                              rearrange in any order."}) 5 |>
  add_card (SurfingLlama 
              {name = "SurfingLlama"; 
               description = "A cool llama dude with a surfboard"}) 4 |>
  add_card (EmoLlama 
              {name = "EmoLlama"; 
               description = "A cool llama dude who only wears black"}) 4 |>
  add_card (CarrotLlama 
              {name = "CarrotLlama"; 
               description = "A cool llama dude who wants to be a carrot"}) 4 |>
  add_card (CodingLlama 
              {name = "CodingLlama"; 
               description = "A cool llama dude who codes"}) 4 |> 
  add_card (HairyLlama 
              {name = "HairyLlama"; 
               description = "A cool llama dude who needs a haircut"}) 4 

(** For a game for 4 players, this will be the deck *)
let deck = 
  build_deck ()

let get_card_name card = 
  match card with
  | Skip {name; description} -> name
  | Attack {name; description} -> name
  | Shuffle {name; description} -> name
  | Nope {name; description} -> name
  | ProphetLlama {name; description} -> name
  | Favor {name; description} -> name
  | Defuse {name; description} -> name
  | Kabllama {name; description} -> name
  | SurfingLlama {name; description} -> name
  | EmoLlama {name; description} -> name
  | CarrotLlama {name; description} -> name
  | HairyLlama {name; description} -> name
  | CodingLlama {name; description} -> name

let get_card_description card = 
  match card with 
  | Skip {name; description} -> description
  | Attack {name; description} -> description 
  | Shuffle {name; description} -> description 
  | Nope {name; description} -> description 
  | ProphetLlama {name; description} -> description 
  | Favor {name; description} -> description 
  | Defuse {name; description} -> description 
  | Kabllama {name; description} -> description 
  | SurfingLlama {name; description} -> description 
  | EmoLlama {name; description} -> description 
  | CarrotLlama {name; description} -> description 
  | HairyLlama {name; description} -> description 
  | CodingLlama {name; description} -> description 

let next_card = function
  | [] -> None
  | h::t -> Some h

let shuffle t = 
  let assign = List.map (fun c -> (Random.bits (), c)) t in
  let st = List.sort compare assign in
  List.map snd st




