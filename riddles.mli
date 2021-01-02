(* The list of riddles *)
type t 
(**The list of tuples containing the options for the corresponding riddle in 
   type t *)
type opt
(**the list of answers correspondings to the riddle in type t *)
type ans

(** [riddles] holds all the riddles for the players *)
val riddles : t

(** [options] holds all the answer choices to their respective riddle in 
    [riddles] *)
val options : opt

(** [answers] holds all the correct answers to their respective riddles in 
    [riddles] *)
val answers : ans

(** [index i lst] returns the element at position [i] *)
val index : int -> 'a list -> 'a

(** gets a random riddle and presents it to the current player. If the player
    gets it right, they get 2 points and if they get it wrong they lose a point. 
    If the player reaches 10 points, they win the game. *)
val random_riddle: Gamestate.game -> Player.player -> t -> opt -> ans -> unit
