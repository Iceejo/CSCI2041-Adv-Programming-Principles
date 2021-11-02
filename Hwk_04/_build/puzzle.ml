(*
********
Strategy
********

Functions I will need:



*)

open Set
open Util

module type PuzzleS = sig
  type st
  val helper : char -> char list * char * char list -> st -> string
  val positions : string -> (char list * char * char list) list
  val answers : string list -> (string * string list) list

end

module PuzzleF (S: SetS) : PuzzleS = struct

  type st = string S.t

  let helper (rare: char)(pos: char list * char * char list)(dict: st): string = 
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

  let answers (word_list: string list) : (string * string list) list = 
    let interest_words = List.filter (fun s -> String.length s >= 5) word_list
    in
    let dict = List.fold_right S.insert word_list S.empty
    in
    let rares = ['j'; 'q'; 'x'; 'z']
    in
    let word_answer (w: string): string * string list = 
      let position = positions w in
      let rec all rs pos = 
        let rec rare r pos = 
          match pos with
          | [] -> []
          | p::rest -> (helper r p dict)::rare r rest
        in
        match rs with
        | [] -> []
        | c::rest -> (rare c pos)@all rest pos
      in
      match (w, all rares position) with
      | (w, ws) -> (w, List.filter ((<>)"") ws)
    in
    List.filter (fun word -> 
                  match word with
                  |(w, ans) -> (<>) ans []) (List.map word_answer interest_words)

end
