(* Part 4 *)

type 'a elf_tree = Empty
                 | Leaf of 'a
                 | Fork of 'a * 'a elf_tree * 'a elf_tree

let t1 : int elf_tree = 
  Fork (3,
        Fork (2, Empty, Leaf 6),
        Fork (4, 
              Leaf 7,
              Fork (5, Empty, Empty)
          )
    )

let t2 : string elf_tree = 
  Fork ("12",
        Fork ("34", Empty, Leaf "567"),
        Fork ("8910", 
              Leaf "1112",
              Fork ("13", Empty, Empty)
          )
    )

let t3 : int elf_tree =
  Fork (1,
        Fork (2, 
              Fork (3, Leaf 0, Leaf 0),
              Fork (4, Leaf 0, Leaf 0) ),
        Fork (5, 
              Fork (6, Leaf 0, Leaf 0),
              Fork (7, Leaf 0, Leaf 0) )
    )

let t4 : 'a elf_tree = Empty

let rec reduce (f: 'a -> 'b -> 'b  -> 'b)(g: 'a -> 'b)(et: 'a elf_tree)(e: 'b): 'b = 
  match et with
  | Empty -> e
  | Leaf x -> g x
  | Fork (v, e1, e2) -> f v (reduce f g e1 e) (reduce f g e2 e)

let size (et: 'a elf_tree): int = 
  reduce (fun v e1 e2 -> 1 + e1 + e2) (fun x -> 1) et 0

let sum (et: int elf_tree): int = 
  reduce (fun v e1 e2 -> v + e1 + e2) (fun x -> x) et 0 

let product (et: int elf_tree): int = 
  reduce (fun v e1 e2 -> v * e1 * e2) (fun x -> x) et 1

let char_count (et: string elf_tree): int = 
  reduce (fun v e1 e2 -> String.length v + e1 + e2) (fun x -> String.length x) et 0

let height (et: 'a elf_tree): int = 
  reduce (fun v e1 e2 -> 1 + max e1 e2) (fun x -> 1) et 0 

let perfect_balance (et: 'a elf_tree): bool = 
  if et = Empty then true
  else
  reduce (fun v e1 e2 -> e1 = e2)(fun x -> true) et false

let maximum (et: int elf_tree): int option = 
  reduce (fun v e1 e2 -> max (max (Some v) e1)(max (Some v) e2))
  (fun x -> Some x) et None
