

let rec all_suffixes (w: 'a list) : 'a list list =
  match w with
  | [] -> [[]]
  | x::xs -> [x::xs]@all_suffixes xs

let all_prefixes (w: 'a list) : 'a list list =
  let rec helper (prev, accum, lst) = 
    match (prev, accum, lst) with
    |([], [], []) -> ([], [[]], [])
    | (p, a, x::xs) -> helper (p@[x], [p@[x]]@accum, xs)
    | (p, a, []) -> (p, a, [])
  in
  match helper ([], [[]], w) with
  | (pre, acc, ls) -> acc

let all_parts (w: 'a list) : ('a list * 'a list) list =
  let rec pair (prefix, suffix, accum) =
    match (prefix, suffix, accum) with
    | ([], [], []) -> ([], [], [])
    |(pre, x::rest, acc) -> pair (pre@[x], rest, (pre@[x], rest)::acc)
    | (pre, suf, acc) -> (pre, suf, acc)
  in
  match pair ([], w, [([], w)]) with
  | (pre, suf, lst) -> lst