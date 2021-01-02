(**
   Parsing of player commands.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.
    Example: if the command is ["play defuse"] then the object_phrase is
    ["defuse"]
    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Play of object_phrase       (* Play card_name *)
  | Describe of object_phrase   (* Describe card_effect *)
  | Show                        (* Shows hand *)
  | Combine of object_phrase    (* Combine two normal cards to snatch *)
  | Pass                        (* Pass turn *)
  | Quit                        (* Quit playing *)
  | Summary
  | Rules

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed


(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    play    defuse   "] is [play ["defuse"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "play", "describe", "show", 
    "combine", "pass", nor "quit" or if the verb is "pass" or "quit" and there
    is a non-empty object phrase, or if the verb is "play", "describe", "show", 
    or "combine" and there is an empty object phrase.*)
val parse : string -> command