(*Group member: Audrey Gasser (gasse038@umn.edu)*)
module type UtilS = sig
  val implode: char list -> string

  val explode: string -> char list

  val read_file: string -> char list

  val split_by: ('a -> bool) -> 'a list -> 'a list list

  val read_words: string -> string list
end

module UtilM : UtilS = struct

  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)

  let explode (s: string) : char list =
    let l = String.length s
    in
    let rec f i = 
      if i = l then [] else s.[i] :: f (i+1)
    in f 0

  let read_file (file_name: string) : char list =
    let ic = open_in file_name 
    in 
    let rec read_chars ic =
      try 
        let next_char = input_char ic
        in next_char :: read_chars ic
      with 
        _ -> [] 
    in read_chars ic

  (*Improvements to split_by: I added comments for each of the match cases to clarify their purpose (Audrey)*)
  let split_by (f: 'a -> bool) (lst: 'a list) : 'a list list = 
    let rec step (next, accum, rest) = 
      match (next, accum, rest) with
      | ([], acc, []) -> ([], acc, []) (*when lst is empty or 
                                        if the last element in rest returned true when f is called*)
      | (n, acc, []) -> (n, acc@[List.rev n], []) (*when we've reached the end of the list*)
      | (n, acc, x::rest) when f x = false -> step (x::n, acc, rest) (*when f x = false*)
      | ([], acc, _::rest) -> step ([], acc, rest) (*when f x = true for the first element 
                                                    or two successive elements in rest*)
      | (n, acc, _::rest) -> step ([], acc@[List.rev n], rest) (*when f x = true*)
    in
    match step ([], [], lst) with
    | (next, accum, l) -> accum

  let read_words (file_name: string) : string list =
    let is_whitespace c = match c with
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false
    in
    List.map implode (split_by is_whitespace (read_file file_name))
end
