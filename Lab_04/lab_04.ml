open Char

let sum_of_floats xs = List.fold_left (+.) 0. xs

let square x = x *. x

let square_floats xs = List.map square xs

let exclaim s = s ^ "!"

let exclaimify strs = List.map exclaim strs

let even x = x mod 2 = 0

let remove_odds xs = List.filter even xs

let nocap str = Char.equal (String.get str 0) (Char.lowercase_ascii (String.get str 0))

let remove_caps strs = List.filter nocap strs