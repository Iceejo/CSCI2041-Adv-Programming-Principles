(* Part 2 *)

type 'a tree 
  = Leaf 
  | Fork of 'a tree * 'a * 'a tree

let t1 : int tree = 
  Fork (Fork (Leaf, 2, Leaf),
        3,
        Fork (Leaf,
              4, 
              Fork (Leaf, 5, Leaf)
          )
    )

let t2 : string tree = 
  Fork (Fork (Leaf, "12", Leaf),
        "34",
        Fork (Leaf,
              "5", 
              Fork (Leaf, "678", Leaf)
          )
    )

let t3 : int tree =
  Fork (Fork (Fork (Leaf, 1, Leaf),
              2, 
              Fork (Leaf, 3, Leaf) ),
        4,
        Fork (Fork (Leaf, 5, Leaf),
              6, 
              Fork (Leaf, 7, Leaf) )
    )

let t4 : 'a tree = Leaf

let rec reduce (f: 'b -> 'a -> 'b -> 'b) (t: 'a tree) (x: 'b): 'b  = 
  match t with
  | Leaf -> x
  | Fork (left, v, right) -> f (reduce f left x) v (reduce f right x)

let size (t: 'a tree): int = 
  reduce (fun lv v rv -> lv + 1 + rv) t 0

let sum (t: int tree): int = 
  reduce (fun lv v rv -> lv + v + rv) t 0

let product (t: int tree): int = 
  reduce (fun lv v rv -> lv * v * rv) t 1

let char_count (t: string tree): int = 
  reduce (fun ls s rs -> ls + String.length s + rs) t 0 

let height (t: 'a tree): int = 
  reduce (fun lv v rv -> 1 + max lv rv) t 1

let perfect_balance (t: 'a tree): bool = 
  if t = Leaf then true
  else
  reduce (fun lv v rv -> lv = rv ) t false

let maximum (t: int tree): int option = 
  reduce (fun lv v rv -> max (max (Some v)(lv))(max (Some v)(rv))) t None
