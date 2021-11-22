# Homework 5: Reasoning about Correctness.
by Kim Ngo

## Problem 1:
```
let rec prod (lst: int list) : int = match lst with
  | [] -> 1
  | y::ys -> y * prod ys
```
### Show: `prod (l1 @ l2) = prod l1 * prod l2`

#### Base case: `l1 = []`
Show: `prod ([] @ l2) = prod [] * prod l2`
```
  prod ([] @ l2)
= prod l2,
  by properties of lists and append
= 0 + prod l2,
  by properties of integers and addition
= prod [] * prod l2,
  by def of prod
```

#### Inductive case: `l1 = x::xs`
Show: `prod ((x::xs) @ l2) = prod (x::xs) * prod l2`

Given: `prod (xs @ l2) = prod xs * prod l2`
```
  prod ((x::xs) @ l2) 
= prod (x :: (xs @ l2)),
  by properties of lists
= x * prod (xs @ l2),
  by def of prod
= x * prod xs * prod l2,
  by inductive hypothesis
= prod (x::xs) * prod l2
  by def of prod
```

## Problem 2:
```
let rec foldr (f: 'a -> 'b -> 'b) (lst: 'a list) (base: 'b) : 'b =
  match lst with
  | [] -> base
  | x::xs -> f x (foldr f xs base)

let prod_fold (lst: int list) : int = foldr ( * ) lst 1
```

### Show: `prod lst = prod_fold lst`

#### Base case: `lst = []`
Show: `prod [] = prod_fold []`
```
  prod []
= 1,
  by def of prod
= foldr ( * ) [] 1,
  by def of foldr
= prod_fold [],
  by def of prod_fold
  ```
#### Inductive case: `lst = x::xs`
Show `prod (x::xs) = prod_fold (x::xs)`

Given `prod xs = prod_fold xs`
```
  prod (x::xs) 
= x * prod xs,
  by def of prod
= x * prod_fold xs,
  by inductive hypothesis
= x * (foldr ( * ) xs 1),
  by def of prod_fold
= foldr ( * ) (x::xs) 1,
  by def of foldr
= prod_fold (x::xs)
  by def of prod_fold
```

## Problem 3:
```
type nat = Zero | Succ of nat

let rec maximum (lst: nat list) : nat = match lst with
  | [] -> Zero 
  | n::ns -> maxnat n (maximum ns)
```
Properties of `maxnat`
```
maxnat Zero n = n

maxnat a (maxnat b c) = maxnat (maxnat a b) c
```

### Show: `maximum (l1 @ l2) = maxnat (maximum l1) (maximum l2)`

#### Base case: `l1 = []`
Show: `maximum ([] @ l2) = maxnat (maximum []) (maximum l2)`
```
  maximum ([] @ l2) 
= maximum l2,
  by properties of lists and append
= maxnat Zero (maximum l2),
  by def of maxnat
= maxnat (maximum []) (maximum l2)
  by def of maximum
```
#### Inductive case: `l1 = x::xs`
Show `maximum ((x::xs) @ l2) = maxnat (maximum (x::xs)) (maximum l2)`

Given `maximum (xs @ l2) = maxnat (maximum xs) (maximum l2)`
```
  maximum ((x::xs) @ l2) 
= maximum (x :: (xs @ l2)),
  by properties of lists
= maxnat x (maximum xs @ l2),
  by def of maximum
= maxnat x (maxnat (maximum xs) (maximum l2)),
  by inductive hypothesis
= maxnat (maxnat x (maximum xs)) (maximum l2),
  by maxnat associative property
= maxnat (maximum (x::xs)) (maximum l2)
  by def of maximum
```

## Problem 4:
### Part 1
```
type 'a tree 
  = Leaf 
  | Fork of 'a tree * 'a * 'a tree
```
The principle of induction for this type is:

for all `t : 'a tree`, *P(*`t`*)* holds if

- *P(*`Leaf`*)*, and
- *P(*`t1`*)*, and *P(*`t2`*)* implies *P(*`Fork(t1, v, t2)`*)* (for any value `v` of type `'a`).

### Part 2
```
type 'a rose_tree = Rose of 'a * 'a rose_tree list
```
The principle of induction for this type is:

for all `t : 'a rose_tree`, *P(*`t`*)* holds if

- *P(*`Rose(v, [])`*)* (for any value `v` of type `'a`), and
- *P(*`rts`*)* implies *P(*`Rose(v, rt::rts)`*)* (for any values `v` of type `'a`).

### Part 3
```
type 'a elf_tree = Empty
                 | Leaf of 'a
                 | Fork of 'a * 'a elf_tree * 'a elf_tree
```
 The principle of induction for this type is:

 for all `t : 'a elf_tree`, *P(*`t`*)* holds if

- *P(*`Empty`*)*,
- *P(*`Leaf v`*)* (for any value `v` of type `'a`), and
- *P(*`t1`*)* and *P(*`t2`*)* implies *P(*`Fork(v, t1, t2)`*)* (for any values `v` of type `'a`).

## Problem 5:
```
type 'a elf_tree = Empty
                 | Leaf of 'a
                 | Fork of 'a * 'a elf_tree * 'a elf_tree

let rec height_rec (t: 'a elf_tree) : int = match t with
  | Empty -> 0
  | Leaf _ -> 1
  | Fork (_, l, r) -> 1 + max (height_rec l) (height_rec r)

let rec reduce (f: 'a -> 'b -> 'b  -> 'b)(g: 'a -> 'b)(t: 'a elf_tree)(e: 'b): 'b = 
  match t with
  | Empty -> e
  | Leaf x -> g x
  | Fork (v, l, r) -> f v (reduce f g l e) (reduce f g r e)

let height (t: 'a elf_tree): int = 
  reduce (fun v l r -> 1 + max l r) (fun x -> 1) t 0 

```
### Show `height_rec t = height t`

#### Base case: `t = Empty`
Show: `height_rec Empty = height Empty`
```
  height-rec Empty
= 0,
  by def of height_rec
= reduce (fun v l r -> 1 + max l r) (fun x -> 1) Empty 0,
  by def of reduce
= height Empty,
  by def of height
```
#### Base case 2: `t = Leaf v`
Show: `height-rec (Leaf v) = height(Leaf v)`
```
  height_rec (Leaf v)
= 1,
  by def of height_rec
= reduce (fun v l r -> 1 + max l r) (fun x -> 1) (Leaf v) 0,
  by def of reduce
= height (Leaf v),
  by def of height
```

#### Inductive case: `t = Fork(v, l, r)`
Show: `height_rec (Fork(v, l, r)) = height (Fork(v, l, r))`

Given: `height_rec l = height l` and `height_rec r = height r`
```
  height_rec (Fork(v, l, r))
= 1 + max (height_rec l) (height_rec r),
  by def of height_rec
= 1 + max (height l) (height r),
  by inductive hypothesis
= 1 + max (reduce (fun v1 l1 r1 -> 1 + max l1 r1) (fun x1 -> 1) l 0) 
    (reduce (fun v2 l2 r2 -> 1 + max l2 r2) (fun x2 -> 1) r2 0),
  by def of height
= reduce (fun v l r -> 1 + max l r) (fun x -> 1) Fork(v, l, r) 0,
  by def of reduce
= height (Fork(v, l, r)),
  by def of height
```
