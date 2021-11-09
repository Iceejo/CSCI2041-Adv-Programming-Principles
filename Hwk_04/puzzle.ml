(*Group member: Audrey Gasser (gasse038@umn.edu)*)
(*Improvements: I originally had my word_answer function nested inside the answers function 
so I moved it outside of the answers function since separating them helps with readability (Audrey)

I renamed the helper function to check_one, I renamed rare to check_rare, and I renamed all to check_all, 
since these names are more informative about the functions' purposes (Audrey)*)
open Set
open Util

module type PuzzleS = sig
  type st
  val check_one : char -> char list * char * char list -> st -> string
  val positions : string -> (char list * char * char list) list
  val word_answer : char list -> st -> string -> string * string list
  val answers : string list -> (string * string list) list

end

module PuzzleF (S: SetS) : PuzzleS = struct

  type st = string S.t
  
  let check_one (rare: char)(pos: char list * char * char list)(dict: st): string = 
    match pos with
    | (pre, c, suf) -> 
      if S.elem (UtilM.implode (pre@[rare]@suf)) dict && rare <> c 
        then UtilM.implode (pre@[rare]@suf)
      else ""

  let positions (w: string): (char list * char * char list) list = 
    let cl = UtilM.explode w in
    let rec position (prefix, suffix, accum) =
      match (prefix, suffix, accum) with
      | ([], [], []) -> ([], [], [])
      | (pre, [], acc) -> (pre, [], acc)
      | (pre, c::suf, acc) -> position (pre@[c], suf, (pre, c, suf)::acc)
    in
    match position ([], cl, []) with
    | (pre, suf, lst) -> lst

  let word_answer (rares: char list)(dict: st)(w: string): string * string list = 
    let position = positions w in
    let rec check_all rs pos = 
      let rec check_rare r pos = 
        match pos with
        | [] -> []
        | p::rest -> (check_one r p dict)::check_rare r rest
      in
      match rs with
      | [] -> []
      | c::rest -> (check_rare c pos)@check_all rest pos
    in
    match (w, check_all rares position) with
    | (w, ws) -> (w, List.filter ((<>)"") ws)

  let answers (word_list: string list) : (string * string list) list = 
    let interest_words = List.filter (fun s -> String.length s >= 5) word_list
    in
    let dict = List.fold_right S.insert word_list S.empty
    in
    let rares = ['j'; 'q'; 'x'; 'z']
    in
    List.filter (fun word -> 
                  match word with
                  |(w, ans) -> (<>) ans []) (List.map (word_answer rares dict) interest_words)

end
