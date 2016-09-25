module Number = struct

  type t = Integer  of int
         | Rational of int * int
         | Float    of float
         | Complex  of float * float

  let gcd a b =
    let rec gcd_rec a b =
      if b = 0 then a else gcd_rec b (a mod b)
    in gcd_rec (abs a) (abs b)

  let lcm a b =
    match a, b with
    | 0, _ | _, 0 -> 0
    | a, b -> abs (a * b) / (gcd a b)
                     
  let float_of_rat rat =
    match rat with
    | Rational (n, d) -> Float ((float_of_int n) /. (float_of_int d))
    | _ -> rat

  let rec simplify rat =
    match rat with
    | Rational (n, 1) -> Integer n
    | Rational (n, d) -> let dv = gcd n d
                         in simplify (Rational (n / dv, d / dv))
    | _ -> rat
                     
  let rec sum x y =
    match (x, y) with
    | (Integer  x, Integer  y) -> Integer (x + y)
    | (Integer  x, Float    y) -> Float ((float_of_int x) +. y)
    | (Float    x, Integer  y) -> Float (x +. (float_of_int y))
    | (Float    x, Float    y) -> Float (x +. y)
    | (Rational _, Float    _) -> sum (float_of_rat x) y
    | (Float    _, Rational _) -> sum x (float_of_rat y)
    | (Rational (nx, dx), Rational (ny, dy)) -> if   dx = dy
                                                then simplify (Rational (nx + ny) dx)
                                                else 
    
end;;
