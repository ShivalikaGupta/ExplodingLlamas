open Deck
open Player

exception Empty

type game = {
  mutable player_list : player list;
  mutable current_player : player;
  mutable game_status : bool;
  mutable deck : card list;
  mutable discard_pile : card list;
}

let update_next_player g = 
  Player.update_turn g.current_player 0;
  g.current_player <- get_np g.current_player;
  Player.update_turn g.current_player 1

let update_current_player g p =
  g.current_player <- p

let check_explode c =
  get_card_name c = "Kabllama"

let check_diffuse p = 
  let rec helper c_list = 
    match c_list with
    | [] -> false
    | h::t -> 
      if get_card_name h = "Diffuse" 
      then true 
      else helper t
  in helper (get_hand p)

let get_player_list g =
  g.player_list

let get_current_player g =
  g.current_player

let get_deck g = 
  g.deck

let update_deck g d =
  g.deck <- d

let draw g = 
  match g.deck with
  | [] -> ()
  | h::t -> begin
      update_deck g t;
      let pn = get_name g.current_player in
      ANSITerminal.(print_string[blue] pn); 
      print_string (" has drawn the ");
      let cn = get_card_name h in 
      if check_explode h 
      then ANSITerminal.(print_string[red] cn)
      else ANSITerminal.(print_string[cyan] cn); 
      print_string (" card! \n");
      add_to_hand g.current_player h
    end

let discard g c = 
  let new_dp = c :: g.discard_pile in 
  g.discard_pile <- new_dp

let rec dealx g p x = 
  if x < 1 
  then () 
  else
    match g.deck with 
    | [] -> ()
    | h::t -> g.deck <- t; 
      add_to_hand p h; 
      dealx g p (x - 1)

let starting_deal g = 
  let rec helper pl =
    match pl with 
    | [] -> ()
    | h::t -> 
      dealx g h 4; 
      add_to_hand h (Defuse 
                       {name = "Defuse"; description = "FIX BOMBS"});
      helper t
  in helper g.player_list

let rec yes_no res =
  if String.equal "yes" res = false && String.equal "no" res = false 
  then 
    let () = print_string ("you inputed an invalid response. 
    Please enter yes or no: ") in
    let new_input = read_line () in
    yes_no new_input
  else
    res

let rec greedy_frugal res =
  if String.equal "greedy" res = false && String.equal "frugal" res = false 
  then 
    let () = print_string ("you inputed an invalid response. 
    Please enter greedy or frugal: ") in
    let new_input = read_line () in
    greedy_frugal new_input
  else
    res

let rec invalid_number number_str =
  try begin
    let number = int_of_string number_str in
    if number < 1 || number > 3 
    then 
      let () = print_string ("You inputed an invalid number. " ^ 
                             "Please enter a number between 1 and 3: ") in
      let new_number = read_line () in
      invalid_number new_number
    else
      number
  end
  with
  | Failure s -> begin
      print_string ("Input was not a number. " 
                    ^ "Please enter a number between 1 and 3: ");
      invalid_number(read_line())
    end

let rec player_check_helper input lst =
  match lst with 
  | [] -> true
  | h::t -> 
    if input = h || input = "" 
    then false 
    else player_check_helper input t

let rec player_check i lst =
  if player_check_helper i lst 
  then i 
  else begin 
    print_string ("Sorry, that name is taken already." ^ 
                  " Please enter a different name: ");
    let input = read_line () in 
    player_check input lst
  end

let ai_description () =
  print_string ("Description of the AIs: \n");
  print_string ("The greedy AI collects as many cards as it can \n");
  print_string ("and the frugal AI plays as many cards as it can! \n")

let player_lst =
  let () = print_string ("Do you want an AI to play with you? (yes / no): ") in
  let answer = yes_no (read_line ()) in
  if String.equal answer "yes" 
  then begin
    ai_description ();
    let () = print_string "how many AIs do you want? (1-3): " in
    let number = invalid_number (read_line ()) in
    if number = 1 
    then 
      let () = print_string ("Do you want the first AI to be greedy_AI or 
        the frugal_AI? (greedy / frugal): ") in
      let answer = greedy_frugal (read_line ()) in
      let p1_name = answer ^ "_computer" in
      let p1 = new_player p1_name None true in
      let pn_lst = p1_name::[] in
      let () = print_string ("Hi player 1! Enter your name: ") in
      let p2_name = player_check (read_line ()) pn_lst in
      let p2 = new_player p2_name (Some p1) false in
      let pn_lst = p2_name::pn_lst in
      let () = print_string ("Hi player 2! Enter your name: ") in
      let p3_name = player_check (read_line ()) pn_lst in
      let p3 = new_player p3_name (Some p2) false in
      let pn_lst = p3_name::pn_lst in
      let () = print_string ("Hi player 3! Enter your name: ") in
      let p4_name = player_check (read_line ()) pn_lst in
      let p4 = new_player p4_name (Some p3) false in
      let () = update_np p1 p4 in
      [p1; p2; p3; p4]
    else if number = 2 
    then
      let () = print_string ("Do you want the first AI to be greedy_AI or 
        the frugal_AI? (greedy / frugal): ") in
      let answer = greedy_frugal (read_line ()) in
      let p1_name = answer ^ "_computer1" in
      let p1 = new_player p1_name None true in
      let pn_lst = p1_name::[] in
      let () = print_string ("Do you want the next AI to be greedy_AI or 
        the frugal_AI? (greedy / frugal): ") in
      let answer = greedy_frugal (read_line ()) in
      let p2_name = answer ^ "_computer2" in
      let p2 = new_player p2_name (Some p1) true in
      let pn_lst = p2_name::pn_lst in
      let () = print_string ("Hi player 1! Enter your name: ") in
      let p3_name = player_check (read_line ()) pn_lst in
      let p3 = new_player p3_name (Some p2) false in
      let pn_lst = p3_name::pn_lst in
      let () = print_string ("Hi player 2! Enter your name: ") in
      let p4_name = player_check (read_line ()) pn_lst in
      let p4 = new_player p4_name (Some p3) false in
      let () = update_np p1 p4 in
      [p1; p2; p3; p4]
    else 
      let () = print_string ("Do you want the first AI to be greedy_AI or 
        the frugal_AI? (greedy / frugal): ") in
      let answer = greedy_frugal (read_line ()) in
      let p1_name = answer ^ "_computer1" in
      let p1 = new_player p1_name None true in
      let pn_lst = p1_name::[] in
      let () = print_string ("Do you want the next AI to be greedy_AI or 
        the frugal_AI? (greedy / frugal): ") in
      let answer = greedy_frugal (read_line ()) in
      let p2_name = answer ^ "_computer2" in
      let p2 = new_player p2_name (Some p1) true in
      let pn_lst = p2_name::pn_lst in
      let () = print_string ("Do you want the final AI to be greedy_AI or 
        the frugal_AI? (greedy / frugal): ") in
      let answer = greedy_frugal (read_line ()) in
      let p3_name = answer ^ "_computer3" in
      let p3 = new_player p3_name (Some p2) true in
      let pn_lst = p3_name::pn_lst in
      let () = print_string ("Hi player! Enter your name: ") in
      let p4_name = player_check (read_line ()) pn_lst in
      let p4 = new_player p4_name (Some p3) false in
      let () = update_np p1 p4 in
      [p1; p2; p3; p4]
  end 
  else 
    let () = print_string ("Hi player 1! Enter your name: ") in
    let p1_name = read_line () in
    let p1 = new_player p1_name None false in
    let pn_lst = p1_name::[] in
    let () = print_string ("Hi player 2! Enter your name: ") in
    let p2_name = player_check (read_line ()) pn_lst in
    let p2 = new_player p2_name (Some p1) false in
    let pn_lst = p2_name::pn_lst in
    let () = print_string ("Hi player 3! Enter your name: ") in
    let p3_name = player_check (read_line ()) pn_lst in
    let p3 = new_player p3_name (Some p2) false in
    let pn_lst = p3_name::pn_lst in
    let () = print_string ("Hi player 4! Enter your name: ") in
    let p4_name = player_check (read_line ()) pn_lst in
    let p4 = new_player p4_name (Some p3) false in
    let () = update_np p1 p4 in
    [p1; p2; p3; p4]

let startstate = 
  let players = player_lst in
  let random_player = List.nth players (Random.int 4) in
  let shuffled_deck = Deck.shuffle Deck.deck in
  let temp_g = 
    { 
      player_list = players;
      current_player = random_player;
      game_status = true;
      deck = shuffled_deck;
      discard_pile = []
    } in
  starting_deal temp_g;
  temp_g

let emptygame = 
  {
    player_list = [];
    current_player = new_player "None" None false;
    game_status = true;
    deck = [];
    discard_pile = []
  }