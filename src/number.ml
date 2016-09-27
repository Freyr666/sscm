exception Not_rational

open Core.Std
        
module Number : sig
  type t =
    Integer of int
  | Rational of int * int
  | Float of float
  | Complex of float * float
  val neg  : t -> t
  val sum  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val div  : t -> t -> t
  val show : t -> string
end = struct
  
  type t = Integer  of int
         | Rational of int * int
         | Float    of float
         | Complex  of float * float

  let gcd_int a b =
    let rec gcd_rec a b =
      if b = 0 then a else gcd_rec b (a mod b)
    in gcd_rec (abs a) (abs b)

  let lcm_int a b =
    match a, b with
    | 0, _ | _, 0 -> 0
    | a, b -> abs (a * b) / (gcd_int a b)
                     
  let float_of_rat rat =
    match rat with
    | Rational (n, d) -> ((float_of_int n) /. (float_of_int d))
    | _ -> raise Not_rational

  let rec simplify rat =
    match rat with
    | Rational (n, 1) -> Integer n
    | Rational (n, d) -> let dv = gcd_int n d
                         in simplify (Rational (n / dv, d / dv))
    | _ -> rat

  let neg x   =
    match x with
    | Integer x       -> Integer (-x)
    | Rational (n, d) -> Rational (-n, d)
    | Float   x       -> Float (-.x)
    | Complex (r, i)  -> Complex (-.r, -.i)
                 
  let sum x y =
    match (x, y) with
    | (Integer x, Integer y) -> Integer (x + y)
    | (Float   x, Float   y) -> Float   (x +. y)
    | (Rational (nx, dx), Rational (ny, dy)) ->
       if   dx = dy
       then simplify (Rational ((nx + ny), dx))
       else let dxy = lcm_int dx dy in
            let mx  = dxy / dx  in
            let my  = dxy / dy  in
            simplify (Rational (((mx * nx) + (my * ny)), dxy))
    | (Complex  (rx, ix), Complex (ry, iy)) ->
       Complex ((rx +. ry), (ix +. iy))
    | (Integer x, Float y)   -> Float ((float_of_int x) +. y)
    | (Float x, Integer y)   -> Float (x +. (float_of_int y))
    | (Rational _, Float y)  -> Float ((float_of_rat x) +. y)
    | (Float x, Rational _)  -> Float (x +. (float_of_rat  y))
    | (Integer x, Rational (ny, dy)) ->
       simplify (Rational ((ny + (x * dy)), dy))
    | (Rational (nx, dx), Integer y) ->
       simplify (Rational ((nx + (y * dx)), dx))
    | (Complex (rx, ix), Integer y)  ->
       Complex ((rx +. (float_of_int y)), ix)
    | (Integer x, Complex (ry, iy))  ->
       Complex ((ry +. (float_of_int x)), iy)
    | (Complex (rx, ix), Float   y)  ->
       Complex ((rx +. y), ix)
    | (Float x,   Complex (ry, iy))  ->
       Complex ((ry +. x), iy)
    | (Complex  (rx, ix), Rational _) ->
       Complex ((rx +. (float_of_rat y)), ix)
    | (Rational _, Complex (ry, iy))  ->
       Complex ((ry +. (float_of_rat x)), iy)

  let sub x y =
    match (x, y) with
    | (Integer x, Integer y) -> Integer (x - y)
    | (Float   x, Float   y) -> Float   (x -. y)
    | (Rational (nx, dx), Rational (ny, dy)) ->
       if   dx = dy
       then simplify (Rational ((nx - ny), dx))
       else let dxy = lcm_int dx dy in
            let mx  = dxy / dx  in
            let my  = dxy / dy  in
            simplify (Rational (((mx * nx) - (my * ny)), dxy))
    | (Complex  (rx, ix), Complex (ry, iy)) ->
       Complex ((rx -. ry), (ix -. iy))
    | (Integer x, Float y)  -> Float ((float_of_int x) -. y)
    | (Float x, Integer y)  -> Float (x -. (float_of_int y))
    | (Rational _, Float y) -> Float ((float_of_rat x) -. y)
    | (Float x, Rational _) -> Float (x -. (float_of_rat  y))
    | (Integer x, Rational (ny, dy)) ->
       simplify (Rational (((x * dy) - ny), dy))
    | (Rational (nx, dx), Integer y) ->
       simplify (Rational ((nx - (y * dx)), dx))
    | (Complex (rx, ix), Integer y)  ->
       Complex ((rx -. (float_of_int y)), ix)
    | (Integer x, Complex (ry, iy))  ->
       Complex (((float_of_int x) -. ry), -.iy)
    | (Complex (rx, ix), Float   y)  ->
       Complex ((rx -. y), ix)
    | (Float   x, Complex (ry, iy))  ->
       Complex ((x -. ry), -.iy)
    | (Complex (rx, ix), Rational _) ->
       Complex ((rx -. (float_of_rat y)), ix)
    | (Rational _, Complex (ry, iy))  ->
       Complex ((ry -. (float_of_rat x)), -.iy)

  let mul x y =
    match (x, y) with
    | (Integer x, Integer y) -> Integer (x * y)
    | (Float   x, Float   y) -> Float   (x *. y)
    | (Rational (nx, dx), Rational (ny, dy)) ->
       simplify (Rational ((nx * ny), (dx * dy)))
    | (Complex  (rx, ix), Complex (ry, iy)) ->
       Complex (((rx *. ry) -. (ix *. iy)), ((rx *. iy) +. (ry *. ix)))
    | (Integer x, Float y)  -> Float ((float_of_int x) *. y)
    | (Float x, Integer y)  -> Float (x *. (float_of_int y))
    | (Rational _, Float y) -> Float ((float_of_rat x) *. y)
    | (Float x, Rational _) -> Float (x *. (float_of_rat  y))
    | (Integer x, Rational (ny, dy)) ->
       simplify (Rational ((x * ny), dy))
    | (Rational (nx, dx), Integer y) ->
       simplify (Rational ((nx * y), dx))
    | (Complex (rx, ix), Integer y)  ->
       let my = float_of_int y in
       Complex ((rx *. my), (ix *. my))
    | (Integer x, Complex (ry, iy))  ->
       let mx = float_of_int x in
       Complex ((mx *. ry), (mx *. iy))
    | (Complex (rx, ix), Float   y)  ->
       Complex ((rx *. y), (ix *. y))
    | (Float   x, Complex (ry, iy))  ->
       Complex ((x *. ry), (x *. iy))
    | (Complex (rx, ix), Rational _) ->
       let my = (float_of_rat y) in
       Complex ((rx *. my), (ix *. my))
    | (Rational _, Complex (ry, iy))  ->
       let mx = (float_of_rat x) in
       Complex ((mx *. ry), (mx *. iy))

  let div x y =
    match (x, y) with
    | (Integer x, Integer y) -> simplify (Rational (x, y))
    | (Float   x, Float   y) -> Float   (x /. y)
    | (Rational (nx, dx), Rational (ny, dy)) ->
       simplify (Rational ((nx * dy), (dx * ny)))
    | (Complex  (rx, ix), Complex (ry, iy)) ->
       let cmod     = (ry *. ry) +. (iy *. iy) in
       let (rc, ic) = (ry, -.iy) in
       Complex (((rx *. rc) -. (ix *. ic)) /. cmod, ((rx *. ic) +. (ry *. ic)) /. cmod)
    | (Integer x, Float y)  -> Float ((float_of_int x) /. y)
    | (Float x, Integer y)  -> Float (x /. (float_of_int y))
    | (Rational _, Float y) -> Float ((float_of_rat x) /. y)
    | (Float x, Rational _) -> Float (x /. (float_of_rat  y))
    | (Integer x, Rational (ny, dy)) ->
       simplify (Rational ((x * dy), ny))
    | (Rational (nx, dx), Integer y) ->
       simplify (Rational (nx, (y * dx)))
    | (Complex (rx, ix), Integer y)  ->
       let my = float_of_int y in
       Complex ((rx /. my), (ix /. my))
    | (Integer x, Complex (ry, iy))  ->
       let cmod     = (ry *. ry) +. (iy *. iy) in
       let (rc, ic) = (ry, -.iy) in
       let mx       = float_of_int x in
       Complex ((mx *. rc) /. cmod, (mx *. ic) /. cmod)
    | (Complex (rx, ix), Float   y)  ->
       Complex ((rx /. y), (ix /. y))
    | (Float   x, Complex (ry, iy))  ->
       let cmod     = (ry *. ry) +. (iy *. iy) in
       let (rc, ic) = (ry, -.iy) in
       Complex ((x *. rc) /. cmod, (x *. ic) /. cmod)
    | (Complex (rx, ix), Rational _) ->
       let my = (float_of_rat y) in
       Complex ((rx /. my), (ix /. my))
    | (Rational _, Complex (ry, iy))  ->
       let cmod     = (ry *. ry) +. (iy *. iy) in
       let (rc, ic) = (ry, -.iy) in
       let mx       = (float_of_rat x) in
       Complex ((mx *. rc) /. cmod, (mx *. ic) /. cmod)

  let show x =
    match x with
    | Integer x -> string_of_int x
    | Float   x -> string_of_float x
    | Rational (n, d) -> (string_of_int n) ^ "/" ^ (string_of_int d)
    | Complex  (r, i) -> (string_of_float r) ^ (if i >= 0. then "+" else "")
                         ^ (string_of_float i) ^ "i"
    
end;;
