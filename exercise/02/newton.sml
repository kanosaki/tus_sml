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

val ** = Math.pow
infix 7 **

fun f1 x = x**3.0 - x;

fun range (st,en,(delta:real)) = 
  if st <= en
    then st :: (range ((st + delta),en,delta))
    else nil

fun rangeout f nil _ _ = (TextIO.flushOut f)
  | rangeout fp (x::xs) fx eps=
    let 
      val y = fx x
      val (y0, r) = newton_d fx eps x
    in
      ( TextIO.output(fp, Real.toString(x)^","^Real.toString(y)^",");
        TextIO.output(fp, Real.toString(y0)^","^Int.toString(r)^"\n");
        print ".";
        rangeout fp xs fx eps
      )
    end

fun openfile s = TextIO.openOut s
val default_range = range(~3.0, 3.0, 0.01)
val small_range = range(0.0, 1.0, 0.1)
      

