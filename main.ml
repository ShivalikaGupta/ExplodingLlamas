open Gamestate
open Command
open Deck
open Player
open Cardeffects
open Riddles

let malformed () =
  let () = print_string ("Your input was malformed. " ^ 
                         "Please try typing the command again: \n") in
  read_line ()

let empty () =
  let () = print_string ("You did not input anything. " ^ 
                         "Please try typing the command again:") in
  read_line ()

let wrong_card str =
  let () = 
    ANSITerminal.(print_string[cyan] str);
    print_string (" is not a card in your hand. Please try typing in a " ^ 
                  "card that is in your hand (you do not need to type in " ^ 
                  "the card twice for combine here): ") in
  read_line ()

let rec get_card_from_str str =
  let player = get_current_player startstate in
  try List.find (fun x -> (get_card_name x) = str) (get_hand player) 
  with 
  | Not_found -> 
    if get_ai_status player
    then raise Command.Malformed
    else get_card_from_str (wrong_card str)

let get_card str =
  List.find (fun x -> (get_card_name x) = str) (get_deck startstate)

let describe t =
  let c = get_card (List.hd t) in 
  let cn = get_card_name c in 
  ANSITerminal.(print_string[cyan] cn);
  let cd = get_card_description c in 
  print_string (": " ^ cd ^ "\n")

let lost_helper g p hand boom_card safe =
  if List.hd hand = boom_card && List.mem safe hand = false
  then begin
    update_status p false; 
    skip g; 
    print_string ("oh no! "); 
    let pn = get_name p in
    ANSITerminal.(print_string[blue] pn);
    print_string(" you lost :( \n")
  end 
  else
  if List.hd hand = boom_card && List.mem safe hand
  then begin 
    print_string("You have successfully "); 
    ANSITerminal.(print_string[green] "Diffused");
    print_string(" the "); 
    ANSITerminal.(print_string[red] "Kabllama");
    print_string("!\n");
    remove_from_hand p boom_card;
    use_card safe g
  end
  else begin 
    check_turn g p
  end  

let lost g =
  let p = get_current_player g in
  let hand = get_hand p in
  let boom_card = Kabllama 
      {name = "Kabllama"; description = "BOOM U DIE"} in
  let safe = Defuse 
      {name = "Defuse"; description = "FIX BOMBS"} in
  lost_helper g p hand boom_card safe

let pass_helper () = 
  draw startstate;
  lost startstate

let check_end () = 
  let players_ingame = List.length (List.filter (fun p -> get_status p = true) 
                                      (get_player_list startstate)) in
  if players_ingame = 1
  then  skip startstate 
  else pass_helper()

let pass () = 
  let players_ingame = List.length (List.filter (fun p -> get_status p = true) 
                                      (get_player_list startstate)) in
  if players_ingame = 2 
  then (random_riddle startstate 
          (get_current_player startstate) riddles options answers; 
        check_end ())
  else  pass_helper () 

let noper player f2 = 
  f2 ();
  print_string (get_name (get_current_player startstate) ^ 
                " your turn has been reversed. sorry\n");
  let nope = Nope 
      {name = "Nope";
       description = "Negate last card played, can be played any time."}in 
  discard startstate nope; 
  remove_from_hand player nope;
  print_string ("Please enter a new command. \n")

let rec yes_no_check str =
  if String.equal str "yes" || String.equal str "no"
  then str
  else let () = print_string ("Your input was malformed." 
                              ^ " Please input yes or no: ") in
    let new_str = read_line () in
    yes_no_check new_str

let rec nope_helper f lst f2 =
  match lst with
  | [] -> f ()
  | h::t -> 
    let cards = get_hand h in
    let nope = Nope 
        {name = "Nope"; 
         description = "Negate last card played, can be played any time."} in
    if List.mem nope cards
    then begin 
      print_string ("Hi ");
      let pn = get_name h in
      ANSITerminal.(print_string[blue] pn); 
      let () = print_string ("! Do you want to use " 
                             ^ "your nope card (yes / no): ") in
      try begin
        let typ_AI = String.sub (get_name h) 0 15 in
        let ai_status = get_ai_status h in
        let status = get_status h in
        if String.equal "frugal_computer" typ_AI && ai_status && status
        then (print_string(get_name h ^ " has said yes\n"); 
              noper h f2)
        else begin
          if String.equal "greedy_computer" typ_AI && ai_status && status
          then (print_string(get_name h ^ " has said no\n"); 
                nope_helper f t f2) 
          else begin let input = yes_no_check (read_line ()) in
            if String.equal "yes" input 
            then noper h f2
            else nope_helper f t f2
          end
        end
      end
      with Invalid_argument s ->
        let input = yes_no_check(read_line ()) in
        if String.equal "yes" input 
        then noper h f2
        else nope_helper f t f2
    end
    else  nope_helper f t f2

let nope_play t = 
  let current_player = get_current_player startstate in
  let p_name = get_name current_player in 
  let players_ingame = List.filter (fun p -> get_status p = true) 
      (get_player_list startstate) in
  let lst = List.filter 
      (fun x -> current_player != x) players_ingame in
  let c = List.hd t in 
  let card = c |> get_card_from_str in
  ANSITerminal.(print_string[blue] p_name); 
  print_string (" just played ");
  ANSITerminal.(print_string[cyan] c); 
  print_string (" \n");
  nope_helper (fun () -> (card |> use_card) startstate) lst 
    (fun () -> remove_from_hand current_player card)
(* nope_helper ((c |> get_card_from_str |> use_card) startstate) lst   *)

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

let p_string t =
  t

(** [pp_string s] pretty-prints string [s]. *)
let pp_string card = 
  match card with 
  | Skip {name; description} -> "\"" ^ name ^ "\""                     
  | Attack {name; description}-> "\"" ^ name ^ "\""                                         
  | Shuffle {name; description} -> "\"" ^ name ^ "\""                                        
  | Nope {name; description} -> "\"" ^ name ^ "\""                                           
  | ProphetLlama {name; description} ->"\"" ^ name ^ "\""                                   
  | Favor {name; description} -> "\"" ^ name ^ "\""                                          
  | Defuse {name; description} -> "\"" ^ name ^ "\""                                          
  | Kabllama {name; description} -> "\"" ^ name ^ "\""                                       
  | SurfingLlama {name; description} -> "\"" ^ name ^ "\""                                   
  | EmoLlama {name; description} -> "\"" ^ name ^ "\""                                       
  | CarrotLlama {name; description} -> "\"" ^ name ^ "\""                                    
  | HairyLlama {name; description} -> "\"" ^ name ^ "\""                                     
  | CodingLlama {name; description} -> "\"" ^ name ^ "\""                                 

let show () = 
  let cards_in_hand = pp_list pp_string 
      (get_hand (get_current_player startstate)) ^ "\n" in
  print_string ("Your hand currently consists of these cards: \n");
  ANSITerminal.(print_string[cyan] cards_in_hand)

let rec players_list acc lst =
  match lst with
  | [] -> acc
  | [h] -> acc ^ get_name h
  | h :: t -> players_list (get_name h ^ ", " ^ acc) t 

let rec hand_sizes acc lst =
  match lst with
  | [] -> acc
  | [h] -> acc ^ (get_name h ^ ": " ^ 
                  string_of_int (h |> get_hand |> List.length)) 
  | h :: t -> hand_sizes (get_name h ^ ": " ^ 
                          string_of_int (h |> get_hand |> List.length) ^ 
                          ", " ^ acc) t

let summary () = 
  let lst = List.filter (fun p -> get_status p = true) 
      (get_player_list startstate) in
  let lst_without_current = 
    List.filter (fun p -> p != get_current_player startstate) lst in
  print_string ("There are " ^ string_of_int (List.length lst) 
                ^ " players left. \n");
  print_string ("They are the following: ");
  print_string ("[");
  let ps = players_list "" lst in 
  ANSITerminal.(print_string[blue] ps);
  print_string  ("] \n") ;
  print_string ("The number of cards that all the other players have is: ");
  print_string ("[");
  print_string (hand_sizes " " lst_without_current);
  print_string ("] \n");
  let cnum = startstate |> get_current_player |> get_hand |> 
             List.length |> string_of_int in
  print_string ("You have " ^ cnum ^ " cards \n");
  print_string ("Here are your current cards : ");
  show ();
  print_string("\n");
  begin
    let players_ingame = List.filter (fun p -> get_status p = true) 
        (get_player_list startstate) in
    let players_ingame_num = List.length players_ingame in 
    if players_ingame_num = 2
    then begin 
      print_string ("you have: " ^ 
                    string_of_int (get_points (get_current_player startstate))^
                    " points");
      let remaining_player = List.filter 
          (fun p -> p != get_current_player startstate) players_ingame in
      let other_player = List.hd remaining_player in
      print_string (get_name other_player ^ "has" ^ 
                    string_of_int(get_points other_player) ^ "points")
    end
    else print_string ("The riddles portion of this game has not started yet. " 
                       ^ "It will start when there are two players remaining.") 
  end

let print_commands () = 
  print_string "Enter: 
  ~ ";
  ANSITerminal.(print_string[green] "play");
  ANSITerminal.(print_string[cyan] " [card name]");
  print_string ": plays any card except for the favor card";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "describe ");
  ANSITerminal.(print_string[cyan] "[card name]");
  print_string ": shows the description / rule for the card name";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "show");
  print_string ": displays the hand of current player";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "combine ");
  ANSITerminal.(print_string[cyan] "[card name 1] [card name 2]");
  print_string ": pick two regular cards [card name 1] [card name 2] to 
  steal a random card from another player";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "summary");
  print_string ": shows all the players' stats (number of players left, 
  the number of cards you have left, the number of cards the other players have
  left, the cards in your hand, and the points each player has if the players 
  are in the riddles portion of the game)";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "rules"); 
  print_string ": explains the rules of the game";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "pass");
  print_string ": draw a card from the top of the deck and end current player's
  turn";
  print_string "
  ~ ";
  ANSITerminal.(print_string[green] "quit");
  print_string ": ends the game \n"

let rec previous_player p lst =
  match lst with
  | [] -> raise (Failure "player not found")
  | h::t -> 
    if get_np h = p 
    then h 
    else previous_player p t

let quit () =
  let current_player = get_current_player startstate in
  let pn = get_name current_player in 
  print_string ("I am sorry to see you go ");
  ANSITerminal.(print_string[blue] pn);
  print_string (":( \n");
  update_status current_player false; 
  skip startstate 

let rec get_random_card lst int =
  match lst with 
  | [] -> failwith "not possible"
  | h::t -> 
    if int = 0 
    then h 
    else get_random_card t (int - 1)

let rec get_alive_player_names lst acc =
  match lst with
  | [] -> acc
  | h::t -> 
    if get_status h && h != get_current_player startstate
    then begin 
      let new_acc = (get_name h)::acc in 
      get_alive_player_names t new_acc
    end
    else get_alive_player_names t acc

let rec combine_check lst pstr =
  let current_player = get_current_player startstate in
  print_string ("What player do you want to steal from?: ");
  ANSITerminal.(print_string[cyan] pstr);
  print_string ("\n");
  let ai_status = get_ai_status current_player in
  if ai_status 
  then begin 
    let random_player = Random.int (List.length lst) in
    index random_player lst 
  end
  else
    let i = read_line () in
    match lst with
    | p1::p2::p3::t -> 
      if i = p1 || i = p2 || i = p3
      then i 
      else begin
        print_string ("You have entered an incorrect player name. " ^ 
                      "Please enter a valid player name. \n"); 
        combine_check lst pstr
      end
    | p1::p2::t -> 
      if i = p1 || i = p2
      then i 
      else begin
        print_string ("You have entered an incorrect player name. " ^ 
                      "Please enter a valid player name. \n"); 
        combine_check lst pstr
      end
    | p1::t -> 
      if i = p1
      then i 
      else begin
        print_string ("You have entered an incorrect player name. " ^ 
                      "Please enter a valid player name. \n"); 
        combine_check lst pstr
      end
    | [] -> failwith ("no way")

let combine t = 
  let current_player = get_current_player startstate in
  let card1 = get_card_from_str (List.hd t) in
  remove_from_hand current_player card1;
  let card2 = get_card_from_str (List.hd (List.tl t)) in
  remove_from_hand current_player card2;
  let p_list_names = get_alive_player_names (get_player_list startstate) [] in
  let playerstr = pp_list p_string p_list_names in 
  let input = combine_check p_list_names playerstr in 
  let player = List.find 
      (fun x -> String.equal (get_name x) input) 
      (get_player_list startstate) in
  let player_hand = get_hand player in
  let card = get_random_card player_hand 
      (Random.int (List.length player_hand)) in
  ANSITerminal.(print_string[blue] (get_name current_player));
  print_string (" just stole "); 
  ANSITerminal.(print_string[cyan] (get_card_name card));
  print_string (" from "); 
  ANSITerminal.(print_string[blue] input);
  print_string ("\n");
  remove_from_hand player card;
  add_to_hand current_player card;
  show();
  print_string("Please enter a new command.\n")

let nope_combine t = 
  let current_player = get_current_player startstate in
  let players_ingame = List.filter (fun p -> get_status p = true) 
      (get_player_list startstate) in
  let lst = List.filter (fun x -> 
      current_player != x)
      players_ingame in
  let c1 = List.hd t in
  let c2 = List.hd (List.tl t) in
  let card1 = get_card_from_str c1 in
  let card2 = get_card_from_str c2 in
  ANSITerminal.(print_string[blue] (get_name current_player));
  print_string (" is combining "); 
  ANSITerminal.(print_string[cyan] c1);
  print_string (" and ");
  ANSITerminal.(print_string[cyan] c2);
  print_string ("\n");
  nope_helper ( fun () -> combine t) lst
    (fun () -> 
       remove_from_hand current_player card1;
       remove_from_hand current_player card2)

let rules () =
  print_string ("The goal of the game is to be the last player alive! \n" ^ 
                "Players will take turn drawing cards from a deck, " ^ 
                "drawing the card ");
  ANSITerminal.(print_string[red] "Kabllama");
  print_string(" will make you explode! (and you will lose). However, you " ^ 
               "are able to save yourself with a "); 
  ANSITerminal.(print_string[green] "Diffuse");
  print_string(" card and the ");
  ANSITerminal.(print_string[red] "Kabllama");
  print_string(" will be put back randomly in the deck! \n You can also win by "
               ^ " getting 10 points in the ");
  ANSITerminal.(print_string[magenta] "Riddles");
  print_string ("portion of the game.\n This will begin when there are only 2 " 
                ^ "players remaining.\n For each riddle you get right, you will"
                ^ " earn 2 points" 
                ^ " and for each you get wrong, you lose a point \n" 
                ^ "In the beginning, there will 4 " 
                ^ "players, and each player will start with a hand of 4 cards " 
                ^ "and a "); 
  ANSITerminal.(print_string[green] "Diffuse");
  print_string(". There will be 3 ");
  ANSITerminal.(print_string[red] "Kabllamas"); 
  print_string(" and 2 extra "); 
  ANSITerminal.(print_string[green] "Diffuses");
  print_string(" in the deck! \n During your turn, you can either "); 
  ANSITerminal.(print_string[green] "play");
  print_string(" a card, "); 
  ANSITerminal.(print_string[green] "combine");
  print_string(" two cards, or "); 
  ANSITerminal.(print_string[green] "pass");
  print_string(". Passing will draw a card and end your turn. Playing a card " ^
               "will activate its effect. \n ");
  ANSITerminal.(print_string[cyan] ("Nope"));
  print_string(": Play this card to negate any card, even when it's not " ^ 
               "your turn! \n "); 
  ANSITerminal.(print_string[cyan] ("Attack"));
  print_string(": Play this card to end your turn. Do not draw any cards." ^ 
               " The next player will be forced to take 2 turns in a row. \n "); 
  ANSITerminal.(print_string[cyan] ("Skip"));
  print_string(": Play this card to end your turn. Do not draw any cards. \n "); 
  ANSITerminal.(print_string[cyan] ("Favor"));
  print_string(": Play this card to force any other player to give you 1 " ^ 
               "card from their hand. They choose which card to give you. \n "); 
  ANSITerminal.(print_string[cyan] ("Shuffle"));
  print_string(": Play this card to shuffle the deck. \n ");
  ANSITerminal.(print_string[cyan] ("ProphetLlama"));
  print_string(": Play this card to view the top 3 cards from the deck." ^ 
               "Then put the top 3 cards back in any order you want. \n ");
  print_string("Regular cards are not able to be played by themselves. " ^ 
               "However, combining two regular cards ("); 
  ANSITerminal.(print_string[cyan] ("SurfingLlama, EmoLlama, CarrotLlama, " ^ 
                                    "HairyLlama, or CodingLlama"));
  print_string(") will allow you to steal a random card from another player." ^ 
               " \n You can "); 
  ANSITerminal.(print_string[green] "play");
  print_string(" or "); 
  ANSITerminal.(print_string[green] "combine");
  print_string(" as many or as few cards as you'd" ^ 
               " like (including none!) during your turn.) ");
  ANSITerminal.(print_string[yellow] "
  TLDR: IF YOU EXPLODE, YOU LOSE. IF YOU DON'T EXPLODE, YOU WIN!
  AND ALL OF THE OTHER CARDS WILL LESSEN YOUR CHANCES OF GETTING EXPLODED.")

let rec command_input input = 
  try
    let command = parse input in
    let () = 
      match command with
      | Quit -> quit ()
      | Play t -> nope_play t
      | Describe t -> describe t
      | Show -> show ()
      | Combine t -> nope_combine t
      | Pass -> pass ()
      | Summary -> summary ()
      | Rules -> rules ()
    in input
  with
  | Command.Malformed -> () |> malformed |> command_input
  | Command.Empty -> () |> empty |> command_input

let command_input_ai input = 
  let command = parse input in
  match command with 
  | Quit -> quit ()
  | Play t -> nope_play t
  | Describe t -> describe t
  | Show -> show ()
  | Combine t -> nope_combine t
  | Pass -> pass () 
  | Summary -> summary () 
  | Rules -> rules ()

let rec turn player =
  let old_input = read_line () in
  let input = command_input old_input in
  if String.equal input "pass" ||
     String.equal input "quit" || 
     String.equal input "play Attack" ||
     String.equal input "play Skip"
  then
    print_string ("turn over. \n") 
  else  
    turn player

let take_turn_greedy () = 
  pass ()

let rec take_turn_frugal_helper hand =
  match hand with
  | [] -> pass ()
  | h::t ->  begin
      try
        let card = get_card_name h in 
        if (String.equal card "SurfingLlama" 
            || String.equal card "EmoLlama"
            || String.equal card "CarrotLlama" 
            || String.equal card "HairyLlama" 
            || String.equal card "CodingLlama") 
        && List.mem h t
        then begin
          command_input_ai ("combine " ^ card ^ " " ^ card);
          take_turn_frugal_helper (get_hand (get_current_player startstate))
        end
        else
        if String.equal card "Attack"
        || String.equal card "Skip"
        then command_input_ai ("play " ^ card)
        else begin
          command_input_ai ("play " ^ card);
          take_turn_frugal_helper t
        end
      with 
      | Command.Malformed -> take_turn_frugal_helper t
      | Command.Empty -> failwith "impossible"
    end  


(* Plays as many cards as possible from the hand *)
let take_turn_frugal () = 
  let current_player = get_current_player startstate in
  let current_hand = current_player |> get_hand in
  let sorted_hand = List.sort compare_card_use current_hand in
  take_turn_frugal_helper sorted_hand

let rec take_turn g =
  let player = get_current_player g in
  let player_name = get_name player in
  let player_status = get_status player in
  try 
    let typ_AI = String.sub player_name 0 15 in
    let ai_status = get_ai_status player in
    if String.equal "frugal_computer" typ_AI && ai_status && player_status
    then begin
      print_string ("It is ");
      ANSITerminal.(print_string[blue] player_name);
      print_string ("'s turn right now. \n");
      show();
      take_turn_frugal ();
      ANSITerminal.(print_string[blue] player_name);
      print_string (" has taken their turn \n")
    end
    else begin 
      if String.equal "greedy_computer" typ_AI && ai_status && player_status
      then begin
        print_string ("It is ");
        ANSITerminal.(print_string[blue] player_name);
        print_string ("'s turn right now. \n");
        show();
        take_turn_greedy ();
        ANSITerminal.(print_string[blue] player_name);
        print_string (" has taken their turn \n")
      end
      else begin 
        if get_status player
        then begin
          print_string ("Hi ");
          ANSITerminal.(print_string[blue] player_name);
          print_string ("! It is your turn: \n");
          print_commands ();
          turn player
        end
        else begin
          skip g;
          take_turn g
        end 
      end 
    end
  with 
  | Invalid_argument s ->
    if player_status
    then begin
      print_string ("Hi ");
      let pn = get_name player in 
      ANSITerminal.(print_string[blue] pn);
      print_string ("! It is your turn: \n");
      print_commands ();
      print_string ("Your cards are: ");
      show ();
      turn player
    end
    else begin
      skip g;
      take_turn g
    end

let rec play_game players_left =
  if players_left <= 1 
  then begin 
    try
      let p_lst = get_player_list startstate in 
      let winner = List.find (fun p -> get_status p = true) p_lst in
      let winner_name = get_name winner in 
      ANSITerminal.(print_string[blue] winner_name);
      print_string (" has won!!! ");
      ANSITerminal.(print_string[yellow] "Congrats!");
      update_status winner false
    with Not_found -> ANSITerminal.(print_string[yellow] "Yay !!")
  end
  else begin
    let players_ingame = List.length (List.filter (fun p -> get_status p = true) 
                                        (get_player_list startstate)) in
    if players_ingame = 1 
    then 
      let p_lst = get_player_list startstate in 
      let winner = List.find (fun p -> get_status p = true) p_lst in
      let winner_name = get_name winner in 
      ANSITerminal.(print_string[blue] winner_name);
      print_string (" has won!!! ");
      ANSITerminal.(print_string[yellow] "Congrats! ");
      update_status winner false
    else begin
      take_turn startstate;
      play_game players_ingame 
    end
  end

let () = play_game 4

