open Deck
open Player
open Gamestate

let rec cardlst_to_string lst acc =
  match lst with
  | [] -> acc
  | [card] -> acc ^ get_card_name card
  | card::t -> cardlst_to_string t (acc ^ (get_card_name card) ^ " ")

let rec get_card card_lst num acc = 
  match card_lst with
  | [] -> failwith "impossible"
  | h::t -> 
    if num = 0
    then (h, (List.rev acc) @ t) 
    else get_card t (num - 1) (h :: acc)

let prophet_llama_print t3 cl = 
  print_string ("Here are the cards you can choose from: ");
  match t3 with 
  | c1::c2::c3::t -> begin
      print_string ("(1) "); 
      let n1 = get_card_name c1 in 
      ANSITerminal.(print_string[cyan] n1);
      print_string (", (2) "); 
      let n2 = get_card_name c2 in 
      ANSITerminal.(print_string[cyan] n2);
      print_string (", (3) "); 
      let n3 = get_card_name c3 in 
      ANSITerminal.(print_string[cyan] n3);
    end
  | c1::c2::t -> begin
      print_string ("(1) "); 
      let n1 = get_card_name c1 in 
      ANSITerminal.(print_string[cyan] n1);
      print_string ("or (2) "); 
      let n2 = get_card_name c2 in 
      ANSITerminal.(print_string[cyan] n2);
    end
  | c1::t -> begin
      print_string ("(1) "); 
      let n1 = get_card_name c1 in 
      ANSITerminal.(print_string[cyan] n1); 
    end
  | [] -> print_string ("")

let rec pl_check l =
  print_string ("Which card should be placed on the deck right now? \n" ^ 
                "1 for the first card, ");
  if l = 2 
  then begin print_string ("or 2 for the next card \n");
    let i = read_line () in 
    if i = "1" || i = "2"
    then i
    else begin print_string ("Sorry, your input is invalid. "); 
      pl_check l
    end
  end
  else begin print_string ("2 for the next card, and 3 for the last card \n");
    let i = read_line () in 
    if i = "1" || i = "2" || i = "3" 
    then i
    else begin print_string ("Sorry, your input is invalid. "); 
      pl_check l
    end
  end

let rec prophet_llama_helper card_lst t =
  match card_lst with
  | [] -> t
  | [c] -> c::t
  | head::tail ->
    let cardlst_str = cardlst_to_string card_lst "" in
    prophet_llama_print card_lst cardlst_str;
    let lst_len = List.length card_lst in 
    let i = (pl_check lst_len |> int_of_string) - 1 in
    let cards_pair = get_card card_lst i [] in
    let card_picked = fst cards_pair in
    let remaining_cards = snd cards_pair in
    prophet_llama_helper remaining_cards (card_picked :: t)

let prophet_llama_end t3 = 
  match t3 with 
  | c1::c2::c3::t -> begin
      print_string ("Top card: "); 
      let n1 = get_card_name c1 in 
      ANSITerminal.(print_string[cyan] n1);
      print_string ("\nSecond card: "); 
      let n2 = get_card_name c2 in 
      ANSITerminal.(print_string[cyan] n2);
      print_string ("\nThird card: "); 
      let n3 = get_card_name c3 in 
      ANSITerminal.(print_string[cyan] n3);
      print_string ("\nFollowed by the rest of the deck. \n"); 
    end
  | c1::c2::t -> begin
      print_string ("Top card: "); 
      let n1 = get_card_name c1 in 
      ANSITerminal.(print_string[cyan] n1);
      print_string ("\nSecond card: "); 
      let n2 = get_card_name c2 in 
      ANSITerminal.(print_string[cyan] n2);
    end
  | c1::t -> begin
      print_string ("Top card: "); 
      let n1 = get_card_name c1 in 
      ANSITerminal.(print_string[cyan] n1); 
    end
  | [] -> print_string ("")

let get_top_three_cards c = 
  match c with
  | c1::c2::c3::tail -> ([c1; c2; c3], tail)
  | c1::c2::[] -> ([c1; c2], [])
  | c1::[] -> ([c1], [])
  | _ -> ([], [])

let prophet_llama g = 
  let top_three = g |> get_deck |> get_top_three_cards in
  update_deck g (prophet_llama_helper (fst top_three) (snd top_three));
  print_string ("You have rearranged the top 3 cards in this order: \n");
  let t3 = g |> get_deck |> get_top_three_cards |> fst in
  prophet_llama_end t3;
  print_string ("Please enter a new command. \n")

(* ranking all the cards based on their usefullness *)  
let rank_card card =
  match card with
  | Skip {name; description} -> 7
  | Attack {name; description} -> 8
  | Shuffle {name; description} -> 4
  | Nope {name; description} -> 9
  | ProphetLlama {name; description} -> 5
  | Favor {name; description} -> 6
  | Defuse {name; description} -> 10
  | Kabllama {name; description} -> 0
  | SurfingLlama {name; description} -> 1
  | EmoLlama {name; description} -> 1
  | CarrotLlama {name; description} -> 1
  | HairyLlama {name; description} -> 1
  | CodingLlama {name; description} -> 1

let compare_card_use card1 card2 =
  let rank1 = rank_card card1 in
  let rank2 = rank_card card2 in
  if rank1 > rank2
  then 1
  else begin
    if rank2 > rank1
    then -1
    else 0
  end

let rec put_together deck sorted_lst =
  match sorted_lst with
  | [] -> deck
  | h::t -> put_together (h :: deck) t

let prophet_llama_ai g =
  let top_three = get_top_three_cards (get_deck g) in
  let top_three_cards = fst top_three in
  let rest_of_deck = snd top_three in
  let top_three_sorted = List.sort compare_card_use top_three_cards in
  let new_deck = put_together rest_of_deck top_three_sorted in
  update_deck g new_deck

let favor_ai_response player =
  let hand = get_hand player in
  let hand_sorted_rank = List.sort compare_card_use hand in
  match hand_sorted_rank with
  | [] -> raise (Failure "no cards in hand")
  | h::t -> h

let rec hand_of_player player_hand acc =
  match player_hand with
  | [] -> acc
  | [h] -> acc ^ get_card_name h
  | h::t -> hand_of_player t (acc ^ get_card_name h ^ ", ")

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc tail = 
      match tail with
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 
        then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let rec favor_pt2_check p1n h str =
  print_string ("What card do you want to give to ");
  ANSITerminal.(print_string[blue] p1n);
  print_string ("?\nYour hand is currently: ");
  ANSITerminal.(print_string[cyan] str);
  let i = read_line () in
  try List.find (fun x -> (get_card_name x) = i) h
  with exn -> 
    print_string ("You have entered an incorrect card name. " ^ 
                  "Please try again. \n");
    favor_pt2_check p1n h str

let favor_second_part p1 p2 =
  let ai_status = get_ai_status p2 in
  try
    let card_to_give = begin
      if ai_status
      then favor_ai_response p2 
      else begin
        (* print_string ("What card do you want to give to ");
           let p_name = get_name p1 in 
           ANSITerminal.(print_string[blue] p_name);
           print_string ("?\nYour hand is currently: ");
           let str = "[" ^ (hand_of_player (get_hand p2) "") ^ "]\n" in
           ANSITerminal.(print_string[cyan] str);
           let i = read_line () in
           List.find (fun x -> (get_card_name x) = i) (get_hand p2)  *)
        let p1n = get_name p1 in 
        let h = get_hand p2 in 
        let str = "[" ^ (hand_of_player (get_hand p2) "") ^ "]\n" in
        favor_pt2_check p1n h str
      end 
    end
    in
    remove_from_hand p2 card_to_give; 
    add_to_hand p1 card_to_give;
    let p1_name = get_name p1 in 
    ANSITerminal.(print_string[blue] p1_name);
    print_string (" got a ");
    let cn = get_card_name card_to_give in
    ANSITerminal.(print_string[cyan] cn);
    print_string(" from "); 
    let p2_name = get_name p2 in 
    ANSITerminal.(print_string[blue] (p2_name ^ "\n"));
    print_string ("Please enter a new command. \n")
  with
  | Failure _ -> print_string ("it looks like player 2 doesn't " ^
                               "have any cards! You wasted your ");
    ANSITerminal.(print_string[cyan] "Favor");
    print_string ("!")

let rec favor_name_helper lst acc =
  match lst with
  | [] -> acc
  | h::t -> 
    if get_status h && h != get_current_player startstate
    then begin 
      let new_acc = (get_name h)::acc in 
      favor_name_helper t new_acc
    end
    else favor_name_helper t acc

let rec favor_check lst pstr =
  print_string ("What player should give you a card?: ");
  ANSITerminal.(print_string[cyan] pstr);
  print_string ("\n");
  let i = read_line () in
  match lst with
  | p1::p2::p3::t -> 
    if i = p1 || i = p2 || i = p3
    then i 
    else begin
      print_string ("You have entered an incorrect player name. " ^ 
                    "Please enter a valid player name. \n"); 
      favor_check lst pstr
    end
  | p1::p2::t -> 
    if i = p1 || i = p2
    then i 
    else begin
      print_string ("You have entered an incorrect player name. " ^ 
                    "Please enter a valid player name. \n"); 
      favor_check lst pstr
    end
  | p1::t -> 
    if i = p1
    then i 
    else begin
      print_string ("You have entered an incorrect player name. " ^ 
                    "Please enter a valid player name. \n"); 
      favor_check lst pstr
    end
  | [] -> failwith ("no way")

let favor g = 
  let p = get_current_player g in
  let p_name = get_name p in
  let () = print_string ("What player should give you a card?: ") in
  let p_list_names = favor_name_helper (get_player_list startstate) [] in
  let playerstr = pp_list (fun x -> x) p_list_names in 
  let p2_name = favor_check p_list_names playerstr in
  let p2 = List.find (fun x -> get_name x = p2_name) (get_player_list g) in
  ANSITerminal.(print_string[blue] p_name);
  print_string (" wants a card from ");
  ANSITerminal.(print_string[blue] p2_name);
  print_string "\n";
  favor_second_part p p2

let rec find_player_max_cards acc card_num current_player player_lst = 
  match player_lst with
  | [] -> acc
  | h::t -> 
    let new_card_num = h |> get_hand |> List.length in
    if new_card_num > card_num && current_player != h
    then find_player_max_cards h new_card_num current_player t
    else find_player_max_cards acc card_num current_player t

let favor_ai g = 
  let player_lst = get_player_list g in
  let player = get_current_player g in
  let p1_name = get_name player in
  let p2 = find_player_max_cards empty_player 0 player player_lst in
  let p2_name = get_name p2 in
  ANSITerminal.(print_string[blue] p1_name);
  print_string (" wants a card from ");
  ANSITerminal.(print_string[blue] (p2_name ^ "\n"));
  favor_second_part player p2

let skip g = 
  let p = get_current_player g in 
  update_turn p 0;
  p |> get_np |> update_current_player g;
  update_turn p 1

let shuffle g = 
  let assign = List.map (fun c -> (Random.bits (), c)) (get_deck g) in
  let st = List.sort compare assign in
  update_deck g (List.map snd st);
  print_string ("The deck has been shuffled! \n");
  print_string ("Please enter a new command. \n")

let attack g =
  update_turn (get_current_player g) 0; 
  skip g;
  update_turn (get_current_player g) 2

let rec insert_card_at t card int = 
  match t with 
  | [] -> [card]
  | h::t as l -> 
    if int = 0 
    then card :: l 
    else h :: (insert_card_at t card (int - 1))

let insert_random_Kabllama g = 
  let deck = get_deck g in
  let boom_card = Kabllama 
      {name = "Kabllama"; description = "BOOM U DIE"} in
  deck |> 
  List.length |> 
  Random.int |> 
  insert_card_at deck boom_card |>
  update_deck g

let check_turn g current_player =
  if get_turn current_player <= 1 
  then skip g 
  else (update_turn current_player 1;
        print_string ("You have to take another turn because someone " ^
                      "attacked you \n"))

let defuse g =
  let current_player = get_current_player g in
  insert_random_Kabllama g;
  check_turn g current_player

let use_card card g =
  let current_player = get_current_player g in
  let ai_status = get_ai_status current_player in
  remove_from_hand current_player card;
  discard g card;
  begin match card with
    | Skip {name; description} -> skip g
    | Attack {name; description} -> attack g
    | Shuffle {name; description} -> shuffle g
    | ProphetLlama {name; description} -> 
      if ai_status
      then prophet_llama_ai g
      else prophet_llama g
    | Favor {name; description} ->
      if ai_status
      then favor_ai g
      else favor g
    | Defuse {name; description} -> defuse g
    | _ -> raise (Failure "card not found") end   
