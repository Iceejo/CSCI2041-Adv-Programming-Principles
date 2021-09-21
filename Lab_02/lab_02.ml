let circle_circum_v1 r = 2. *. r *. 3.14

let circle_circum_v2 r = let pi = 3.14 in
    2. *. r *. pi

let rec power x y= 
    if x = 0 then 1.
    else if x = 1 then y
    else y *. power (x-1) y

let cube y = power 3 y