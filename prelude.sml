
(* Utility functions *)

fun curry f x y = f(x,y)

fun $ (f,g) = f g
infixr 0 $

fun foldl_i _ nil = raise Empty 
  | foldl_i f (x::xs) = foldl f x xs 

fun foldr_i _ nil = raise Empty 
  | foldr_i f (x::xs) = foldr f x xs 
