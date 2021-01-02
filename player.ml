open Deck

type player = {
  name : string;
  mutable turn : int;
  mutable status : bool;
  is_ai : bool;
  mutable points : int;
  mutable hand : card list;
  mutable next_player : player option;
}

let empty_player = {
  name = "";
  turn = 0;
  status = true;
  is_ai = true;
  points = 0;
  hand = [];
  next_player = None
}

let new_player n np ai_status =
  {name = n;
   turn = 0;
   status = true; 
   is_ai = ai_status; 
   points = 0;
   hand = []; 
   next_player = np;}

let add_points p =
  p.points <- p.points + 2

let get_points p =
  p.points 

let deduct_points p =
  p.points <- p.points - 1

let get_name p =
  p.name

let get_turn p =
  p.turn

let update_turn p i = 
  p.turn <- i

let get_status p =
  p.status

let get_ai_status p =
  p.is_ai

let update_status p s =
  p.status <- s

let get_hand p =
  p.hand

let add_to_hand p c = 
  p.hand <- c::p.hand

let rec remove_helper card handlst acc = 
  match handlst with
  | [] -> acc
  | h::t -> 
    if h = card 
    then List.rev acc @ t
    else remove_helper card t (h :: acc)

let remove_from_hand p c =
  p.hand <- remove_helper c (get_hand p) []

let get_np p = 
  match p.next_player with
  | None -> failwith "what the heck"
  | Some person -> person

let update_np p1 p2 = 
  p1.next_player <- Some p2

