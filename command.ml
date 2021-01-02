open Gamestate
open Cardeffects

type object_phrase = string list

type command = 
  | Play of object_phrase       (* Play card_name *)
  | Describe of object_phrase   (* Describe card_effect *)
  | Show                        (* Shows hand *)
  | Combine of object_phrase    (* Combine two normal cards to snatch *)
  | Pass                        (* Pass turn *)
  | Quit                        (* Quit playing *)
  | Summary                     (*Shows stats of the game *)
  | Rules                 (*Shows the rules of the game *)

exception Empty

exception Malformed

let non_empties lst =
  List.filter (fun x -> (String.equal x "") = false) lst 

let check_all_card t = 
  let card = List.hd t in
  String.equal card "Skip" 
  || String.equal card "Attack" 
  || String.equal card "Shuffle" 
  || String.equal card "ProphetLlama" 
  || String.equal card "Favor" 
  || String.equal card "SurfingLlama" 
  || String.equal card "EmoLlama"
  || String.equal card "CarrotLlama" 
  || String.equal card "HairyLlama" 
  || String.equal card "CodingLlama"
  || String.equal card "Kabllama"
  || String.equal card "Defuse"
  || String.equal card "Nope"

let check_card t =
  let card = List.hd t in
  String.equal card "Skip" 
  || String.equal card "Attack" 
  || String.equal card "Shuffle" 
  || String.equal card "ProphetLlama" 
  || String.equal card "Favor" 

let check_card_combine t = 
  let card = List.hd t in 
  String.equal card "SurfingLlama" 
  || String.equal card "EmoLlama"
  || String.equal card "CarrotLlama" 
  || String.equal card "HairyLlama" 
  || String.equal card "CodingLlama"

let parse_helper lst = 
  match lst with
  | [] -> raise Empty
  | h::t -> 
    if String.equal h "quit" then Quit 
    else if String.equal h "pass" then Pass 
    else if String.equal h "show" then Show
    else if String.equal h "summary" then Summary
    else if String.equal h "rules" then Rules
    else if String.equal h "combine" then Combine t 
    else if String.equal h "describe" then Describe t 
    else Play t

let check lst =
  match lst with 
  | [] -> raise Empty
  | h::t -> if (String.equal h "play") = false 
            && (String.equal h "describe") = false 
            && (String.equal h "show") = false
            && (String.equal h "combine") = false 
            && (String.equal h "pass") = false
            && (String.equal h "quit") = false 
            && (String.equal h "summary") = false
            && (String.equal h "rules") = false
    then raise Malformed 
    else if ((String.equal h "quit") 
             || (String.equal h "pass") 
             || (String.equal h "show")
             || (String.equal h "summary")
             || (String.equal h "rules"))
         && t <> [] 
    then raise Malformed 
    else if ((String.equal h "play") 
             && (t = [] || check_card t = false))
    then raise Malformed
    else if ((String.equal h "describe") && (t = [] || 
    check_all_card t = false))
    then raise Malformed
    else if String.equal h "combine" && 
            (List.length t <> 2 
             || check_card_combine t = false
             || List.hd t <> List.hd (List.tl t)) 
    then raise Malformed 
    else parse_helper lst

let parse str = 
  let lst_words = non_empties (String.split_on_char ' ' str) in 
  check lst_words