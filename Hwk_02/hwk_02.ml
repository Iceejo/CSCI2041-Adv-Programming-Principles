let sum (xs: int list): int = List.fold_left (+) 0 xs

let sumf (xs: float list): float = List.fold_left (+.) 0. xs

let product (xs: int list): int = List.fold_left ( * ) 1 xs

let productf (xs: float list): float = List.fold_left ( *. ) 1. xs

let minimum (xs: 'a list): 'a = 
  match xs with
  | [] -> raise (Invalid_argument "minimum: empty list as input")
  | x::rest -> List.fold_left (fun current next -> if current > next then next else current) x rest

let longest (strs: string list): string = 
  match strs with
  | [] -> raise (Invalid_argument "longest: empty list as input")
  | x::rest -> List.fold_left (fun s1 s2 -> if String.length s2 > String.length s1 then s2 else s1) x rest

let append (lst1: 'a list) (lst2: 'a list): 'a list = 
  List.fold_right (fun l1 l2 -> l1::l2) lst1 lst2

let elem (a: 'a) (lst: 'a list): bool = 
  List.fold_right (fun x sofar ->
                    if x=a && sofar = false then not sofar
                    else sofar) lst false

let excited (strs: string list): string list = 
  List.map (fun s -> s ^ "!") strs

let suffix (s: string) (strs: string list): string list = 
  let punc = s in
  List.map (fun str -> str ^ punc) strs

let excited' (strs: string list): string list = suffix "!" strs

let lengths (strs: string list): int list = 
  List.map String.length strs

let length_pairs (strs: string list): (string * int) list = 
  List.map (fun s -> (s, String.length s)) strs

let capitalize (strs: string list): string list = 
  List.map String.capitalize_ascii strs

let all_odds (xs: int list): int list = 
  let odd x = x mod 2 = 1 in
  List.filter odd xs

let all_capitalized (strs: string list): string list = 
  let cap str = Char.equal (String.get str 0) (Char.uppercase_ascii (String.get str 0)) 
  in
  match strs with
  |[] -> []
  | _::""::_ -> raise(Invalid_argument "all_capitalized: list contains empty string")
  | _ -> List.filter cap strs

let all_squares (xs: int list): int list = 
  let is_square x = 
    let rec square_check v n = 
      if n * n = x then true
      else if n * n > x then false
      else square_check x (n + 1)
    in
    square_check x 0
  in
  List.filter is_square xs

let group (lst: 'a list): ('a * 'a) list = 
  let pair x (xs, temp, position) =
    if position mod 2 = 0
    then (xs, x::temp, position + 1)
    else 
      match (xs, temp, position) with
      | (x1, x2::_, p) -> ( ((x, x2)::x1), [], p + 1)
      | _ -> (xs, temp, position)
  in
  if List.length lst mod 2 = 1
    then raise (Invalid_argument "group: list length odd")
  else match List.fold_right pair lst ([], [], 0) with
       | (grp, temp, pos) -> grp

let unzip (lst: ('a * 'b) list): ('a list * 'b list) =
  List.fold_right (fun (x, y) (x', y') -> (x::x', y::y')) lst ([], [])
