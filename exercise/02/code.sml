
fun curry f x y = f (x, y);

fun newton f eps = 
let 
  fun until p ch x = if p x then x else until p ch (ch x)
  fun sat x = Real.abs( f(x) ) < eps
  fun deriv f x dx = (f(x + dx) - f(x)) / dx
  fun imp x  = x - (f(x)/(deriv f x eps))
in until sat imp
end;

fun newton_d f eps = 
let 
  fun until p ch n x  = if p x then (x, n) else until p ch (n+1) (ch x) 
  fun sat x = Real.abs( f(x) ) < eps
  fun deriv f x dx = (f(x + dx) - f(x)) / dx
  fun imp x  = x - (f(x)/(deriv f x eps))
in until sat imp 0
end;

fun take_while p nil = nil
  | take_while p (x::xs) = if p x then x::take_while p xs else nil;
fun drop_while p nil = nil
  | drop_while p (x::xs) = if p x then drop_while p xs else x::xs;

fun insert (x, xs) = 
  (take_while (curry op>= x) xs) @ [x] @ (drop_while (curry op>= x) xs);
fun isort nil = nil
  | isort xs = foldr insert nil xs;


datatype intOrReal = I of int | R of real;

infix <!
fun (I x) <! (I y) = x < y
  | (I x) <! (R y) = real(x) < y
  | (R x) <! (I y) = x < real(y)
  | (R x) <! (R y) = x < y;

datatype 'a LIST = Cons of 'a * 'a LIST | Nil;

fun Append (xs,Nil) = xs
  | Append (Nil,ys) = ys
  | Append ((Cons (x, xs)),ys) = Cons (x, Append (xs,ys));



exception NotANumber
exception ParseError;

fun take_to nil _ = raise ParseError
  | take_to (x::xs) sep = if x = sep then nil else x::(take_to xs sep)
fun drop_to nil _ = raise ParseError
  | drop_to (x::xs) sep = if x = sep then xs else drop_to xs sep;

fun ctoi c = 
let val value = ord(c) - ord(#"0")
in if value < 0 orelse value > 9 
      then raise NotANumber 
      else value 
end;

fun conv_int nil = raise NotANumber
  | conv_int (#"~"::xs) = ~1 * conv_int xs
  | conv_int expr = 
  let
    val foldfunc = foldl (fn (x,y) => x + 10*y) 0
  in
    foldfunc (map ctoi expr)
  end

fun parse_cl nil = nil
  | parse_cl ((#"n")::(#"i")::(#"l")::_) = nil
  | parse_cl ((#"[")::body) = parse_cl body
  | parse_cl ((#"]")::_) = nil
  | parse_cl exprs = 
  let 
    val expr = take_to exprs #"," 
      handle ParseError => take_to exprs #"]"
    val remain = drop_to exprs #"," 
      handle ParseError => nil
  in (conv_int expr) :: parse_cl remain
  end

fun parse s = parse_cl (explode s)
