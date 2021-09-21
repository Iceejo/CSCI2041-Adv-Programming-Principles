open Char
open String

let rec sum_of_floats xs = 
  match xs with
  | [] -> 0.
  |x::rest -> x +. sum_of_floats rest

let rec square_floats xs = 
  match xs with
  |[]->[]
  |x::rest -> x*.x::square_floats rest

let rec exclaimify xs = 
  match xs with
  | [] -> []
  | s::rest -> (s ^ "!") :: exclaimify rest

let rec remove_odds xs = 
  match xs with
  | [] -> []
  | x::rest when x mod 2 = 0 -> x::remove_odds rest
  | x::rest -> remove_odds rest

let rec remove_caps xs = 
  match xs with
  | [] -> []
  | x::rest -> if Char.equal (String.get x 0) (Char.lowercase_ascii (String.get x 0)) then x::remove_caps rest else remove_caps rest