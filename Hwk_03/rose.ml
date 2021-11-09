
(* Part 3. Rose trees *)

type 'a rose_tree = Rose of 'a * 'a rose_tree list

let t1 : int rose_tree =
  Rose (2, [ Rose (1, [ Rose (5, []); Rose (7, []) ] );
               Rose (3, []);
               Rose (4, []);
             ]   )

let t2 : string rose_tree = 
  Rose ("r", [ Rose ("abc", [ Rose ("xy", []); Rose ("z", []) ] );
               Rose ("hello", []);
               Rose ("goodbye", [])
             ]
    )

let t3 : char rose_tree = 
  Rose ('r', [ Rose ('a', [ Rose ('m', []); Rose ('n', []) ] );
               Rose ('b', [ Rose ('o', []); Rose ('p', []) ] );
               Rose ('c', [ Rose ('x', []); Rose ('y', []) ] )
             ]
    )

let t4 : int rose_tree = Rose (4, [])

let rec reduce (f: 'a -> 'b list -> 'b)(rt: 'a rose_tree): 'b = 
  match rt with
  | Rose (v, rt_lst) -> f v (List.map (reduce f) rt_lst)

let size (rt: 'a rose_tree): int = 
  reduce (fun v lst -> 1 + List.fold_right (+) lst 0) rt

let sum (rt: int rose_tree): int = 
  reduce (fun v lst -> v + List.fold_right (+) lst 0) rt 

let product (rt: int rose_tree): int = 
  reduce (fun v lst -> v * List.fold_right ( * ) lst 1) rt

let char_count (rt: string rose_tree): int = 
  reduce (fun s lst -> String.length s + List.fold_right (+) lst 0) rt

let height (rt: 'a rose_tree): int =
  reduce (fun v lst -> 1 + List.fold_right max lst 0)  rt 

let perfect_balance (rt: 'a rose_tree): bool = 
  (*let sub x (prev, balanced) =
    if balanced = false 
      then (prev, false)
    else
      match x with
      | Rose (v, l) when List.length l = List.length prev -> (l, true)
      | Rose (v, l) -> (l, false)
  in
  match (reduce (fun v lst -> List.fold_right (sub) lst ([], true)) rt) with
  | (x, bal) -> bal*)
  match rt with
  | Rose (_, []) -> true
  | _ -> reduce (fun v lst -> List.fold_right (=) lst false) rt


let maximum (rt: int rose_tree): int option = 
  reduce (fun v lst -> max (Some v) (List.fold_right (max) lst (Some v))) rt
