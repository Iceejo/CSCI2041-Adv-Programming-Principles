open Set

module type  TreeS = sig
  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  val empty : 'a t

  val insert : 'a -> 'a t -> 'a t

  val elem : 'a -> 'a t -> bool

  val height: 'a t -> int

  val size: 'a t -> int

end

module TreeM : TreeS = struct

  type 'a tree = Leaf
               | Fork of 'a tree * 'a * 'a tree

  type 'a t = 'a tree

  let empty = Leaf

  let rec insert (e: 'a)(t: 'a t): 'a t = 
    match t with
    | Leaf -> Fork (Leaf, e, Leaf)
    | Fork (left, v, right) ->
        if e = v then t
        else if e < v then Fork (insert e left, v, right)
        else Fork (left, v, insert e right)

  let rec elem (e: 'a)(t: 'a t): bool = 
    match t with
    | Leaf -> false
    | Fork (left, v, right) ->
        if e = v then true
        else if e < v then elem e left
        else elem e right

  let height _ = raise (Failure "complete this")

  let size _ = raise (Failure "complete this")

end

module TreeSet : SetS = TreeM
