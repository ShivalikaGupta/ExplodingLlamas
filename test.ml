open OUnit2
open Command
open Deck
open Player
open Cardeffects
open Gamestate
open Riddles


(* Test Plan: 
   Our test cases tests for all the public functions in cardeffects.ml, 
   command.ml, deck.ml, gamestate.ml, player.ml, and riddles.ml(all files 
   except for main.ml). For functions in main.ml such as nope card, we manually 
   tested since it was difficult to have nope implemented outside of the main.ml 
   file. We ommitted testing some functions that update the player, deck, or 
   gamestate because we set up pre-assigned sets of decks and gamestate to test 
   using getter functions. We believe that our test suite demonstrate the 
   correctness of our system because we used both glass box testing and black 
   box testing. During the beginning development stage of the game, we used 
   black box testing by writing test cases that fail first and see where the 
   code is performing wrong. During the later development stage of the game, 
   we started using glass box testing to ensure that the implementation is 
   right by testing each type of card effects and commands in different 
   environments to test for edge cases. 
*)


(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort String.compare lst1 in
  let uniq2 = List.sort String.compare lst2 in
  uniq1 = uniq2

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [pp_string s] pretty-prints string [s]. *)
let pp_string card = 
  match card with 
  | Skip {name = name ; description = description} -> 
    "\"" ^ "Skip" ^ "\""                     
  | Attack {name = name ; description = description}-> 
    "\"" ^ "Attack" ^ "\""                                         
  | Shuffle {name = name ; description = description} -> 
    "\"" ^ "Shuffle" ^ "\""                                        
  | Nope {name = name ; description = description} -> 
    "\"" ^ "Nope" ^ "\""                                           
  | ProphetLlama {name = name ; description = description} ->
    "\"" ^ "ProphetLlama" ^ "\""                                   
  | Favor {name = name ; description = description} -> 
    "\"" ^ "Favor" ^ "\""                                          
  | Defuse {name = name ; description = description} -> 
    "\"" ^ "Defuse" ^ "\""                                          
  | Kabllama {name = name ; description = description} -> 
    "\"" ^ "Kabllama" ^ "\""                                       
  | SurfingLlama {name = name ; description = description} -> 
    "\"" ^ "SurfingLlama" ^ "\""                                   
  | EmoLlama {name = name ; description = description} -> 
    "\"" ^ "EmoLlama" ^ "\""                                       
  | CarrotLlama {name = name ; description = description} -> 
    "\"" ^ "CarrotLlama" ^ "\""                                    
  | HairyLlama {name = name ; description = description} -> 
    "\"" ^ "HairyLlama" ^ "\""                                     
  | CodingLlama {name = name ; description = description} -> 
    "\"" ^ "CodingLlama" ^ "\""                                 

(* Deck.ml tests *)

let shuffle_test 
    (name : string) 
    (g : Gamestate.game) 
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_deck g))

let next_card_test
    (name : string)
    (t : Deck.t) 
    (expected_output : Deck.card option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (next_card t))

let get_card_name_test
    (name : string)
    (t : Deck.card) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_card_name t))

let get_card_description_test 
    (name : string)
    (t : Deck.card) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_card_description t) 
      ~printer:String.escaped)

(* cardeffects.ml tests *)

let prophet_helper g =
  prophet_llama g; 
  Gamestate.get_deck g 

let prophet_llama_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (prophet_helper g))

let rank_card_test
    (name : string)
    (card : Deck.card) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (rank_card card) ~printer:string_of_int)

let compare_card_use_test 
    (name : string)
    (card1 : Deck.card) 
    (card2 : Deck.card)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (compare_card_use card1 card2) 
        ~printer:string_of_int)

let skip_helper g = 
  skip g;
  get_name(Gamestate.get_current_player g)

let skip_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (skip_helper g) ~printer:String.escaped)

let attack_helper g = 
  attack g;
  get_name(Gamestate.get_current_player g)

let attack_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (attack_helper g) ~printer:String.escaped)

let favor_helper g = 
  favor g;
  let current_player = Gamestate.get_current_player g in
  get_card_name(List.hd(get_hand current_player))

let favor_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (favor_helper g) ~printer:String.escaped)

let defuse_helper g = 
  let current_player = Gamestate.get_current_player g in
  update_status current_player false;
  defuse g;
  get_status current_player

let defuse_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (defuse_helper g))

let insert_card_at_test
    (name : string)
    (t : Deck.t) 
    (card : Deck.card)
    (int : int)
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (insert_card_at t card int))

(*testing cards*)
let atk = Attack
    {name = "Attack"; 
     description = "End Turn. Next Player takes two turns"}

let shuf = Shuffle 
    {name = "Shuffle"; description = "Shuffle Deck"}

let fvr = Favor 
    {name = "Favor"; description = "Pick another player. They give you any 
    card they choose."}
let prophet_llama = ProphetLlama 
    {name = "ProphetLlama"; 
     description = "Look at top 3 cards, rearrange in any order."}

(*testing decks*)
let test_deck = 
  [atk; shuf; shuf]

let insert_Shuffle = 
  [atk; shuf; shuf; shuf]

let shuffled_top_three_deck = 
  [shuf; atk; prophet_llama]

let prophet_llama_deck = 
  [shuf; atk; prophet_llama; prophet_llama; prophet_llama]

let top_three_cards = 
  (test_deck,shuf)

let test_deck_favor = 
  [atk; shuf; fvr]

let after_stolen_deck = 
  [shuf; shuf]

(*player.ml tests*)

(*player.ml: startstate tests*)
let rec get_names player_lst acc =
  match player_lst with
  | [] -> acc
  | h::t -> get_names t ((Player.get_name h) :: acc)

let names_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_names (get_player_list g) []))

let rec get_num_cards player_lst acc =
  match player_lst with
  | [] -> acc
  | h::t -> let p_hand = Player.get_hand h in
    let ph_length = List.length p_hand in
    get_num_cards t (ph_length :: acc)

let count_start_hand_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_num_cards (get_player_list g) []))

let next_player_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_name(get_np (get_current_player g)))
        ~printer:String.escaped)

let get_status_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_status(get_current_player g)))

let add_to_hand_helper g = 
  let current_player = Gamestate.get_current_player g in
  add_to_hand current_player atk;
  List.length (get_hand current_player)

let add_to_hand_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (add_to_hand_helper g) 
        ~printer:string_of_int)

let remove_from_hand_helper g = 
  let current_player = Gamestate.get_current_player g in
  remove_from_hand current_player atk;
  List.length (get_hand current_player)

let remove_from_hand_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (remove_from_hand_helper g) 
        ~printer:string_of_int)

let get_turn_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_turn (get_current_player g)) 
        ~printer:string_of_int)

(*player.ml: points test*)
let add_points_helper g = 
  let current_player = Gamestate.get_current_player g in
  let current_point = get_points current_player in
  add_points current_player;
  (get_points current_player) - current_point

let add_points_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (add_points_helper g)
        ~printer:string_of_int)

let deduct_points_helper g = 
  let current_player = Gamestate.get_current_player g in
  let current_point = get_points current_player in
  deduct_points current_player;
  (get_points current_player) - current_point

let deduct_points_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (deduct_points_helper g)
        ~printer:string_of_int)

let get_points_helper g = 
  let current_player = Gamestate.get_current_player g in
  get_points current_player

let get_points_test 
    (name : string)
    (g : Gamestate.game) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_points_helper g)
        ~printer:string_of_int)

(*riddle tests*)

let riddles_helper g = 
  random_riddle g (get_current_player g) riddles options answers;
  get_points (get_current_player g)

let riddle_test 
    (name : string)
    (g: Gamestate.game)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (riddles_helper g))

let riddle_win_helper g = 
  let players_ingame = List.filter (fun p -> get_status p = true) 
      (get_player_list startstate) in
  List.length players_ingame

let riddle_win_test
    (name : string)
    (g: Gamestate.game)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (riddle_win_helper g)
      ~printer:string_of_int)


let test_gamestate = 
  Gamestate.startstate 

let dummy_gamestate_prophet = 
  Gamestate.emptygame

let test_empty_player = 
  Player.empty_player

let startstate_test =
  [
    names_test "test correct inputs a b c d" 
      test_gamestate ["d"; "c"; "b"; "a"];
    count_start_hand_test "Each player starts with 5" 
      test_gamestate [5; 5; 5; 5];
    next_player_test "check whether next player is correct" test_gamestate "d";
    get_status_test "checks whether player is still in the game" test_gamestate
      true;
    add_to_hand_test "add attack card to current player's hand" test_gamestate
      6;
    remove_from_hand_test "remove attack card to current player's hand" 
      test_gamestate 5;
    get_turn_test "player that just went gets 0 turn" test_gamestate 0;

  ]
let points_test = 
  [
    get_points_test "get point value of player at start state" test_gamestate 0;
    add_points_test "add 1 point to start state" test_gamestate 2;
    deduct_points_test "deduct 1 point from start state" test_gamestate (-1);
  ]

let () = update_deck dummy_gamestate_prophet prophet_llama_deck

let prophet_lamma_tests = 
  [ 
    prophet_llama_test "change it so its reverse, inputs 1 1" 
      dummy_gamestate_prophet
      [prophet_llama; atk; shuf; prophet_llama; prophet_llama];
    prophet_llama_test "change it back to what it was inputs 1 1" 
      dummy_gamestate_prophet
      prophet_llama_deck;
  ]
let skip_dummy_player_non_ai g =
  get_name(List.nth (Gamestate.get_player_list g) 3)

let attack_dummy_player_non_ai g =
  get_name(List.nth (Gamestate.get_player_list g) 2)

let skip_tests = 
  [
    skip_test "skip: all regular player" 
      test_gamestate (skip_dummy_player_non_ai test_gamestate);
  ]

let attack_tests = 
  [
    attack_test "attack: all regular player"
      test_gamestate (attack_dummy_player_non_ai test_gamestate);
  ]

let favor_tests = 
  [
    favor_test "favor: all regular player"
      test_gamestate "Defuse";
  ]

let defuse_tests = 
  [
    defuse_test "defuse: all regular player"
      test_gamestate false;
  ]

let rank_card_tests = 
  [
    rank_card_test "rank card: Attack"
      atk 8;
    rank_card_test "rank card: Favor"
      fvr 6;
    rank_card_test "rank card: Favor"
      shuf 4;
  ]

let compare_card_use_tests = 
  [
    compare_card_use_test "compare card use: Attack vs Favor"
      atk fvr 1;
  ]

let () = shuffle test_gamestate

let new_deck = get_deck test_gamestate

let string_new_deck = 
  List.map (fun x -> get_card_name x) (Deck.shuffle Deck.deck)

let string_deck = 
  List.map (fun x -> get_card_name x) (Deck.deck)

let deck_tests =
  [
    insert_card_at_test "insert_card_test_1" test_deck shuf 1 insert_Shuffle;
    "Testing shuffle- same elements" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists string_new_deck (string_deck)); 
    "Testing shuffle checking if different order of elements" >:: (fun _ -> 
        assert_bool "lists are same order" (new_deck != Deck.deck));
    next_card_test "next_card_test_1" test_deck (Some atk); 
    get_card_name_test "card name: Attack" atk("Attack");
    get_card_description_test "card description: Attack" atk 
      ("End Turn. Next Player takes two turns");
  ]

(** [parse_test name str expected_output] is an OUnit test case named [name] 
    for [parse str] asserting that the output is [expected_output]. *)
let parse_test 
    (name : string) 
    (str : string) 
    (expected_output : command) : test =
  name >:: fun ctxt -> 
    assert_equal expected_output (parse str) 

let parse_errors 
    (name : string) 
    (str : string) 
    (expected_output : exn) : test =
  name >:: fun ctxt ->
    assert_raises expected_output (fun () -> parse str)

let command_tests = [
  parse_test "Testing whether play command works for parse" 
    "play    Attack" (Play ["Attack"]) ;
  parse_test "Testing wether describe command words for parse" 
    "  describe Defuse" (Describe ["Defuse"]);
  parse_test "Testing whether show command works for parse" 
    "  show" (Show);
  parse_test "Testing whether combine command works for parse" 
    "  combine SurfingLlama     SurfingLlama" 
    (Combine ["SurfingLlama"; "SurfingLlama"]);
  parse_test "Testing wether quit command works for parse" 
    "quit" Quit;
  parse_test "Testing wether pass command works for parse" 
    "pass" Pass;
  parse_test "Testing wether pass command works for parse" 
    "summary" Summary;
  parse_test "Testing wether pass command works for parse" 
    "rules" Rules;
  parse_errors "Testing wether empty is rasied for parse" 
    "" Command.Empty;
  parse_errors "Testing wether malformed is raised for 
    having non-empty string after quit"  
    "quit Defuse" Malformed;
  parse_errors "Testing wether malformed is raised for 
    having empty string after play" 
    "play " Malformed; 
  parse_errors "testing wether malformed is raised for 
    having a mispelled card name after play" 
    "play Fvor" Malformed;
  parse_errors "testing wether malformed is raised for 
    having cards that are not llama cards after combine" 
    "command Defuse Defuse" Malformed;
  parse_errors "testing wether Malfromed is raised for 
    having two different card names after combine" 
    "command SurfingLlama SurfingLlam" Malformed;
  parse_errors "testing wether malformed is raised for 
  having only one card after combine" 
    "command SurfingLlama" Malformed
]

let riddle_tests = 
  [
    riddle_test "startstate: riddle correct: 2 pts" startstate 2;
    riddle_test "startstate: riddle wrong: 1 pts" startstate 1;
    riddle_test "startstate: riddle correct 2: 3 pts" startstate 3;
    riddle_test "startstate: riddle correct 3: 5 pts" startstate 5;
    riddle_test "startstate: riddle correct 4: 7 pts" startstate 7;
    riddle_test "startstate: riddle correct 5: 9 pts" startstate 9;
    riddle_test "startstate: riddle wrong 2: 8 pts" startstate 8;
    riddle_test "startstate: riddle correct 6: 10 pts" startstate 10;
    riddle_win_test "no one left in the game after one player 
    reaches 10 pts" startstate 0;
  ]
let suite =
  "test suite for A2"  >::: List.flatten [
    points_test;
    startstate_test;
    prophet_lamma_tests;
    deck_tests;
    skip_tests; 
    command_tests;
    attack_tests;
    favor_tests;
    defuse_tests;
    rank_card_tests;
    compare_card_use_tests;
    riddle_tests;
  ]

let _ = run_test_tt_main suite

