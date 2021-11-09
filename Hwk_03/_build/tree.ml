(* Part 1 *)

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

let rec size (t: 'a tree): int = 
  match t with
  | Leaf -> 0
  | Fork (left, v, right) -> size left + 1 + size right

let rec sum (t: int tree): int = 
  match t with
  | Leaf -> 0
  | Fork (left, n, right) -> sum left + n + sum right

let rec product (t: int tree): int = 
  match t with
  | Leaf -> 1
  | Fork (left, n, right) -> product left * n * product right

let rec char_count (t: string tree): int = 
  match t with
  | Leaf -> 0
  | Fork (left, s, right) -> char_count left + String.length s + char_count right

let rec height (t: 'a tree): int = 
  match t with
  | Leaf -> 1
  | Fork (left, v, right) -> 1 + max (height left) (height right)

let rec perfect_balance (t: 'a tree): bool = 
  match t with
  | Leaf -> true
  | Fork (left, v, right) -> 
    height left = height right && perfect_balance left && perfect_balance right

  let rec maximum (t: int tree): int option =
    match t with
    | Leaf -> None
    | Fork (left, v, right) -> 
      max (max (Some v)(maximum left))(max (Some v)(maximum right))
