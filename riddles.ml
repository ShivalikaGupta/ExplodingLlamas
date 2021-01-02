open Player
open Gamestate
type t = string list
type opt = string list list
type ans = string list

let riddles = 
  ["I am a mother's child and a father's child but nobody's son. What am I?";
   "16 - 4 / (1/4) + 2";
   "When I get multiplied by any number, the sum of the figures in the product
   is always me. What am I?";
   "What ten letter word starts with gas?";
   "Marvin was 13 years old in 1870 and 8 years old in 1875. " ^ 
   "How is it possible?";
   "What flies when it's born, lies when it's alive, and runs when it's dead?";
   "If POST is 1234 and FLIRT is 56784, what is FROST?";
   "I start with M, end with X, and have never ending amount of letters." ^ 
   "What am I?";
   "Rebecca weighs 98 pounds plus half her own weight. " ^ "
   How much does she weigh?";
   "In Roman Numerals, how many hours are in a day?";
   "What comes once in a minute, twice in a moment, " ^ "
   but never in a thousand years?";
   "What is 3/7 chicken, 2/3 cat and 2/4 goat?";
   "I am a 7 letter word. I become longer when my third letter is removed. " ^ "
   Who am I?";
   "Which word best expresses the meaning of 'Reassure'?";
   "A painting and a sculpture cost $1500 in total. " ^ 
   "The painting costs $1000 morethan the sculpture. " ^ 
   "How much does the Sculpture cost?"
  ]

let options =
  [
    ["orphan"; "gradson"; "father"; "daughter"]; 
    ["6"; "17"; "5"; "2"];
    ["9"; "8"; "2"; "4"];
    ["Retirement"; "Automobile"; "Aberration"; "Television"];
    ["The dates are in AC"; "1875 was a leap year";
     "1870 began on the vernal equinox"; "The dates are in BC"];
    ["A grain of sand"; "An eaglet"; "A snowflake"; "A fruit fly"];
    ["58234"; "58243"; "52384"; "43285"];
    ["Mix"; "Mailbox"; "Multiplex"; "Matrix"];
    ["128"; "196"; "192"; "184"];
    ["XXVI"; "XXIV"; "XXIL"; "XIXV"];
    ["Thiry-one seconds"; "1/1000 of a decade"; "One-tenth of a century"; 
     "The Letter 'M'"];
    ["Childbirth"; "Chiffon"; "Chicago"; "Chipmunk"];
    ["Longing"; "Lounger"; "Lengthy"; "Longine"];
    ["Proffer"; "Hearten"; "Indce"; "Submit"];
    ["$400"; "$450"; "$250"; "$200"]
  ]

let answers =
  [
    "D"; "D"; "A"; "B"; "D"; "C"; "A"; "B"; "B"; "B"; "D"; "C"; "B"; "B"; "C"
  ]

let rec check_abcd st =
  let str = String.uppercase_ascii st in
  if String.equal str "A" = false 
  && String.equal str "B" = false
  && String.equal str "C" = false
  && String.equal str "D" = false
  then let () = print_string ("your input was malformed. " ^ 
                              "Please enter A, B, C, or D: " ) in
    let response = read_line () in
    check_abcd response
  else str

let rec index i lst =
  match lst with 
  | [] -> failwith "Not found"
  | h :: t -> if i = 0 then h else index (i - 1) t

let rec abcd_format acc tup =
  match tup with
  | [] -> acc
  | h :: t -> begin
      if List.length t = 3
      then abcd_format (acc ^ "A: " ^ h ^ "\n") t
      else if List.length t = 2 
      then abcd_format (acc ^ "B: " ^ h ^ "\n") t
      else if List.length t = 1 
      then abcd_format (acc ^ "C: " ^ h ^ "\n") t
      else abcd_format (acc ^ "D: " ^ h ^ "\n") t
    end

let check_winner g p= 
  if get_points p >= 10 
  then let lst = List.filter (fun p -> p != get_current_player g) 
           (get_player_list g) in
    List.iter (fun p -> update_status p false) lst
  else print_string ("You are now" ^
                     "  at a total of " ^ string_of_int (get_points p)
                     ^ " points and you need " ^ 
                     string_of_int (10 - get_points p) ^ " to win\n")

let ai_riddles g p answer=
  let rand_ans = Random.int 4 in
  let ai_ans = index rand_ans ["A"; "B"; "C"; "D"] in
  if String.equal ai_ans answer then begin (add_points p); 
    print_string (get_name p ^ " chose " ^ ai_ans ^ 
                  " and chose the right answer! "); check_winner g p end 
  else begin deduct_points p; 
    print_string (get_name p ^ " got the answer wrong. The correct answer is " 
                  ^ answer ^ "\n") end


let random_riddle g p t options ans =
  let rand = Random.int 15 in
  print_string("your riddle is the following:\n");
  print_string (index rand t ^ "\n");
  print_string (abcd_format "" (index rand options));
  let answer = index rand ans in
  if get_ai_status p
  then ai_riddles g p answer
  else
    let () = print_string ("\n Enter A, B, C, or D: ") in
    let response = check_abcd (read_line ()) in
    if String.equal response answer 
    then begin (add_points p); 
      print_string ("You got the correct answer!\n"); check_winner g p end
    else begin (deduct_points p); 
      print_string ("Sorry, your answer was wrong. The correct answer is " 
                    ^ answer ^ "\n")
    end