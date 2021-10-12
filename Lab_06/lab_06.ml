(*Group members: Elizabeth Grondahl, grond039@umn.edu*)

let rec unzip xs = 
  match xs with
  | [] -> ([], [])
  | (x1, x2)::rest -> 
    let (x1_next, x2_next) = unzip rest in 
      (x1::x1_next, x2::x2_next)

(*
Improvements to unzip: 
In my second pattern, I originally had (x1', x2'), 
which I replaced with (x1_next, x2_next) since this is more descriptive and understandable.
*) 

let group (lst: 'a list): ('a * 'a) list = 
  let pair x (xs, hold) =
    match hold with
    | [] -> (xs, x::hold)
    | [prev_x] -> ( (x, prev_x)::xs, [])
    | _ -> (xs, hold)
  in
  if List.length lst mod 2 = 1
    then raise (Invalid_argument "group: list length odd")
  else 
    match List.fold_right pair lst ([], []) with
       | (grp, hld) -> grp

(*
Improvements to group: 
I originally wrote my pair function as let pair x (xs, temp, position). 
- I got rid of the position counter in pair function because it's redundant and unnecessary (Elizabeth Grondahl)
- I renamed temp as hold to improve understandability (Elizabeth Grondahl)

I originally wrote pair using if-then-else statements so I changed it to a match statement to increase conciseness (Elizabeth Grondahl)
*)