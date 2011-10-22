
type loc = real * real

fun dist ((x0, y0), (x1, y1)) = let
  val dx = x1 - x0
  val dy = y1 - y0
in
  Math.sqrt (dx * dx + dy * dy)
end 

fun heron (a, b, c) = let
  val ab = dist (a ,b)
  val bc = dist (b, c)
  val ac = dist (a, c)
  val perim = ab + bc + ac
  val s = perim / 2.0
in
  Math.sqrt (s * (s - ab) * (s - bc) * (s - ac))
end

datatype shape
  = Circle of loc * real
  | Square of loc * real
  | Triangle of loc * loc * loc

fun area (Circle (_, r)) = Math.pi * r * r
  | area (Square (_, s)) = s * s
  | area (Triangle (a, b, c)) = heron (a, b, c)

(*
* fun area shape = 
*   case shape
*       of Circle (_, r) => Math.pi * r * r
*        | Square (_, s) => s * s
*        | Triangle (a, b, c) => heron (a, b, c)
* *)

fun center (Circle (c, _)) = c
  | center (Square ((x, y), s)) = (x + s / 2.0, y + s / 2.0)

