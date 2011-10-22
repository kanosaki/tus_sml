
fun curry f x y = f (x, y);

fun newton f eps = 
let 
  fun until p ch x = if p x then x else until p ch (ch x)
  fun sat x = Real.abs( f(x) ) < eps
  fun deriv f x dx = (f(x + dx) - f(x)) / dx
  fun imp x  = x - (f(x)/(deriv f x eps))
in until sat imp
end;

fun take_while p nil = nil
  | take_while p (x::xs) = if p x then x::take_while p xs else nil;
fun drop_while p nil = nil
  | drop_while p (x::xs) = if p x then drop_while p xs else x::xs;

fun insert (x, xs) = 
  (take_while (curry op>= x) xs) @ [x] @ (drop_while (curry op>= x) xs);
fun isort nil = nil
  | isort xs = foldr insert nil xs;

exception ParseError;

fun take_to nil _ = raise ParseError
  | take_to (x::xs) sep = if x = sep then nil else x::(take_to xs sep)
fun drop_to nil _ = raise ParseError
  | drop_to (x::xs) sep = if x = sep then xs else drop_to xs sep;

datatype intOrReal = I of int | R of real;

infix <!
fun (I x) <! (I y) = x < y
  | (I x) <! (R y) = real(x) < y
  | (R x) <! (I y) = x < real(y)
  | (R x) <! (R y) = x < y;

datatype 'a LIST = Cons of 'a * 'a LIST | Nil;

fun append xs Nil = xs
  | append Nil ys = ys
  | append (Cons (x, xs)) ys = Cons (x, append xs ys);


