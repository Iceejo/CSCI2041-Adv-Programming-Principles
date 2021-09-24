let rec sum xs = 
  match xs with
  | [] -> 0
  | x::rest -> x + sum rest

let rec sumf xs = 
  match xs with
  | [] -> 0.
  | x::rest -> x +. sumf rest

let rec product xs = 
  match xs with
  | [] -> 1
  | x::rest -> x * product rest

let rec productf xs = 
  match xs with
  | [] -> 1.
  | x::rest -> x *. productf rest

let rec minimum xs = 
  match xs with
  | [] -> raise(Invalid_argument "minimum")
  | x::[] -> x
  | x1::x2::rest when x1 < x2 -> minimum (x1::rest) 
  | x1::x2::rest -> minimum (x2::rest)

let rec longest xs = 
  match xs with
  | [] -> raise(Invalid_argument "longest")
  | x::[] -> x
  | x1::x2::rest when (String.length x1) >= (String.length x2) -> longest(x1::rest) 
  | x1::x2::rest -> longest (x2::rest)

let rec append (xs)(xs') = 
  match xs with
  | [] -> [] @ xs'
  | hd::tl -> hd::tl @ xs'

let rec elem (a)(xs) = 
  match xs with
  | [] -> false
  | x::rest -> a=x || elem (a)(rest)

let rec excited xs = 
  match xs with
  | [] -> []
  | x::rest -> (x ^ "!") :: excited rest

let rec suffix s xs = 
  match xs with
  | [] -> []
  | x::rest -> (x ^ s)::suffix s rest

let rec excited' xs = suffix "!" xs

let rec lengths xs =
  match xs with
  | [] -> []
  | x::rest -> (String.length x)::(lengths rest)

let rec length_pairs xs =
  match xs with
  | [] -> []
  | x::rest -> (x, String.length x)::(length_pairs rest)

let rec capitalize xs = 
  match xs with
  | [] -> []
  | x::rest -> (String.capitalize_ascii x)::(capitalize rest)

let rec all_odds xs = 
  match xs with
  | [] -> []
  | x::rest when x mod 2 = 0 -> all_odds rest 
  | x::rest -> x::all_odds rest 

let rec all_capitalized xs = 
  match xs with
  | [] -> []
  | ""::_ -> raise(Invalid_argument "all_capitalized")
  | x::rest when Char.equal (String.get x 0) (Char.lowercase_ascii (String.get x 0)) -> all_capitalized rest 
  | x::rest -> x::all_capitalized rest

let rec square x n =
  if n * n = x then true
  else if n * n > x then false
  else square x (n + 1)

let rec all_squares xs = 
  match xs with
  | [] -> []
  | x::rest when square x 0 = true -> x::all_squares rest
  | _::rest -> all_squares rest

let rec group xs = 
  match xs with
  | [] -> []
  | x::[] -> raise (Invalid_argument "group")
  | x1::x2::rest -> (x1, x2)::group rest

let rec unzip xs = 
  match xs with
  | [] -> ([], [])
  | (x1, x2)::rest -> 
    let (x1', x2') = unzip rest in 
      (x1::x1', x2::x2')