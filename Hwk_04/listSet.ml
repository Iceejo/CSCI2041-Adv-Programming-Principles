open Set

module ListM : SetS = struct

  type 'a t = 'a list

  let empty = []

  let insert (e: 'a)(lst: 'a t): 'a t = e::lst

  let elem (e: 'a)(lst: 'a t): bool = 
    List.fold_right (fun x sofar ->
      if x=e && sofar = false then not sofar
      else sofar) lst false

end
