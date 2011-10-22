(* 3 7 11 21 31 41 *)

fun curry f x y = f (x, y)

(* 3 *)
exception Error
fun drop 0 xs      = xs 
  | drop _ []      = []
  | drop n (x::xs) = if n<0 then raise Error else drop (n-1) xs 

(* 7 *)
exception ListError
fun butlast []      = raise ListError
  | butlast (x::[]) = []
  | butlast (x::xs) = x::(butlast xs);
 
(* 11 m *)
fun rev_itlist f [] b = b
  | rev_itlist f (x::xs) b = rev_itlist f xs (f x b);

(* 21 *)
fun flat [] = []
  | flat ([]::xs) = flat xs
  | flat ((x::xs)::ys) = x::(flat (xs::ys));

(* 31 *)
fun addstring s []      = []
  | addstring s (x::xs) = (s^x)::(addstring s xs);

fun addstring' s = map (curry op^ s)

(* 41 *)
fun foldl2 f a [] = a
  | foldl2 f a (x::xs) = foldl2 f (f(x, a)) xs; 

fun uncurry f (x, y) = f x y;
fun rev_itlist2 f xs b = foldl2 (uncurry f) b xs;

fun foldl3 f a xs = rev_itlist (curry f) xs a;

fun drop2 n xs = 
  if n < 0 
    then raise Error
    else 
      if n = 0
        then xs
        else drop (n-1) (tl xs)

