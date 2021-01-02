open OUnit2
open Command
open Deck
open Player
open Cardeffects
open Gamestate
open Riddles

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort String.compare lst1 in
  let uniq2 = List.sort String.compare lst2 in
  uniq1 = uniq2

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

let insert_card_at_test
    (name : string)
    (t : Deck.t) 
    (card : Deck.card)
    (int : int)
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (insert_card_at t card int))

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

let prophet_helper g =
  prophet_llama g; 
  Gamestate.get_deck g 

let prophet_llama_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : Deck.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (prophet_helper g))

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

let ai_status_test
    (name : string)
    (g : Gamestate.game) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_ai_status (get_current_player g)))

(*cardeffects.ml*)
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

let test_gamestate = 
  Gamestate.startstate 

let startstate_test =
  [
    names_test "test correct inputs a b greedy_computer1 greedy_computer2" 
      test_gamestate ["b"; "a"; "greedy_computer2"; "greedy_computer1"];
    count_start_hand_test "Each player starts with 5" 
      test_gamestate [5; 5; 5; 5];
    ai_status_test "check whether player is AI or not" test_gamestate true;
  ]

let dummy_gamestate_prophet = 
  Gamestate.emptygame

let () = update_deck dummy_gamestate_prophet prophet_llama_deck

let prophet_lamma_tests = 
  [ 
    prophet_llama_test "change it so its reverse, inputs 1 1" 
      dummy_gamestate_prophet
      [prophet_llama; atk; shuf; prophet_llama; prophet_llama];
  ]
let skip_dummy_player_ai g =
  get_name(List.nth (Gamestate.get_player_list g) 3)

let attack_dummy_player_ai g =
  get_name(List.nth (Gamestate.get_player_list g) 2)

let skip_tests = 
  [
    skip_test "skip: 3 AI" 
      test_gamestate (skip_dummy_player_ai test_gamestate);
  ]
let attack_tests = 
  [
    attack_test "attack: 3 AI"
      test_gamestate (attack_dummy_player_ai test_gamestate);
  ]

let favor_tests = 
  [
    favor_test "favor: 3 AI"
      test_gamestate "SurfingLlama";
  ]
let defuse_tests = 
  [
    defuse_test "defuse: all regular player"
      test_gamestate false;
  ]

let () = shuffle test_gamestate
let new_deck = get_deck test_gamestate
let string_new_deck = 
  List.map (fun x -> get_card_name x) (Deck.shuffle Deck.deck)
let string_deck = 
  List.map (fun x -> get_card_name x) (Deck.deck)

let deck_tests =
  [
    (* TODO: add tests for the State module here *)
    insert_card_at_test "insert_card_test_1" test_deck shuf 1 insert_Shuffle;
    "Testing shuffle- same elements" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists string_new_deck (string_deck)); 
    "Testing shuffle checking if different order of elements" >:: (fun _ -> 
        assert_bool "lists are same order" (new_deck != Deck.deck));
    next_card_test "next_card_test_1" test_deck (Some atk); 
  ]

let suite =
  "test suite for A2_AI"  >::: List.flatten [
    startstate_test;
    favor_tests;
    prophet_lamma_tests;
    deck_tests;
    skip_tests; 
    attack_tests;
    defuse_tests;
  ]

let _ = run_test_tt_main suite

