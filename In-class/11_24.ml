let rec squares_from (n: int) : int stream = 
  print_endline ("step square: " ^ string_of_int n);
  Cons (n*n, delay (fun () -> squares_from (n+1)))

let rec filter (f: 'a -> bool) (s: 'a stream) : 'a stream = 
  match s with
  | Cons (h, t) ->
    let rest = delay (fun () -> filter f (demand t))
    in
    if f h then
      Cons (h, rest)
    else
      demand rest