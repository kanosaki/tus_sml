fun curry f x y = f (x, y)
exception ListError
exception Error
exception EmptyList
exception NotFound
exception LengthError
exception InsertListError
(* 1 *)
fun funpow 0 f a = a
  | funpow n f a = 
      if n <= 0 
        then 0 
        else f (funpow (n-1) f a)

(* 2 *)
fun append xs [] = xs
  | append [] ys = ys
  | append (x::xs) ys = x::(append xs ys)

(* 3 *)
fun drop 0 xs = xs
  | drop _ [] = raise Error 
  | drop n (x::xs) = drop (n-1) xs

(* 4 *)
fun replace _ _ [] = []
  | replace 0 a (x::xs) = a::(replace ~1 a xs)
  | replace m a (x::xs) = x::(replace (m-1) a xs)

(* 5 *)
fun last []  = raise ListError
  | last [x] = x
  | last (x::xs)  = last xs

(* 6 *)
fun last2 [] = raise ListError
  | last2 [_] = raise ListError
  | last2 [x, _] = x
  | last2 (x::xs) = last2 xs

(* 7 *)
fun butlast []      = raise ListError
  | butlast (x::[]) = []
  | butlast (x::xs) = x::(butlast xs)

(* 8 *)
fun butlast2 [] = raise ListError
  | butlast2 [_] = raise ListError
  | butlast2 [_, y] = [y] 
  | butlast2 (x::xs) = x::(butlast2 xs)

(* 9 *)
fun replicate _ 0 = []
  | replicate a n = 
    if n > 0 
      then a::(replicate a (n-1))
      else raise Error

(* 10 *)
fun itlist f [] b = b
  | itlist f (x::xs) b = f x (itlist f xs b)

(* 11 *)
fun rev_itlist f [] b = b
  | rev_itlist f (x::xs) b = rev_itlist f xs (f x b);

(* 12 *)
fun end_itlist _ [] = raise EmptyList
  | end_itlist f [x] = x
  | end_itlist f (x::xs) = f x (end_itlist f xs)

(* 13 *)
fun map _ [] = []
  | map f (x::xs) = (f x)::(map f xs)

(* 14 *)
fun find _ [] = raise NotFound
  | find p (x::xs) = 
    if p x
      then x
      else find p xs

(* 15 *)
fun find2 _ [] = raise NotFound
  | find2 p (x::xs) = 
      if p x
        then []
        else x::(find2 p xs)

(* 16 *)
fun assoc _ [] = raise NotFound
  | assoc a ((x,y)::xys) = 
      if a = x
        then y
        else assoc a xys

(* 17 *)
fun assoc2 _ [] = raise NotFound
  | assoc2 a ((x,y)::xys) = 
      if a = x
        then (x, y)
        else assoc2 a xys

(* 18 *)
fun rev_assoc _ [] = raise NotFound
  | rev_assoc b ((x,y)::xys) = 
      if b = y
        then x
        else rev_assoc b xys

(* 19 *)
fun rev_assoc2 _ [] = raise NotFound
  | rev_assoc2 b ((x,y)::xys) = 
      if b = y
        then (x, y)
        else rev_assoc2 b xys

(* 20 *)
fun filter _ [] = []
  | filter p (x::xs) = 
      if p x 
        then x::(filter p xs) 
        else filter p xs

(* 21 *)
fun flat [] = []
  | flat ([]::xs) = flat xs
  | flat ((x::xs)::ys) = x::(flat (xs::ys));

(* 22 *)
fun combine [] [] = []
  | combine xs [] = raise LengthError
  | combine [] ys = raise LengthError
  | combine (x::xs) (y::ys) = (x, y)::(combine xs ys)

(* 23 *)
fun split [] = ([],[])
  | split ((x, y)::vs) = (x::(#1(split vs)), y::(#2(split vs)))

(* 24 *)
fun forall _ [] = true
  | forall p (x::xs) = (p x) andalso (forall p xs)

(* 25 *)
fun exists _ [] = false
  | exists p (x::xs) = (p x) orelse (exists p xs)

(* 26 *)
fun member _ [] = false
  | member a (x::xs) = (a = x) orelse (member a xs)

(* 27 *)
fun take 0 _ = []
  | take n [] = if n > 0 then raise Error else []
  | take n (x::xs) = x::(take (n-1) xs)


(* 28 *)
fun $ (l, r) a = l(r(a))
infix $

(* 29 *)
fun mullist [] []     = []
  | mullist (_::_) [] = raise LengthError
  | mullist [] (_::_) = raise LengthError
  | mullist (x::xs) (y::ys) = (x*y)::(mullist xs ys)

(* 30 *)
fun add2list [] = raise LengthError
  | add2list [_] = raise LengthError
  | add2list (x::y::vs) = (x + y)::(add2list (y::vs))

(* 31 *)
fun addstring s []      = []
  | addstring s (x::xs) = (s^x)::(addstring s xs)

(* 32 *)
fun rev_sub [] = 0
  | rev_sub [x] = x
  | rev_sub (x::xs) = (rev_sub xs) - x

(* 33 *)
fun addlist k 0 xs = k::xs
  | addlist k _ [] = raise Error
  | addlist k n (x::xs) = 
      if n < 0 
        then raise Error 
        else x::(addlist k (n-1) xs)

(* 34 *)
fun dellist _ []      = raise Error
  | dellist 0 (x::xs) = xs
  | dellist n (x::xs) = 
    if n < 0
      then raise Error
      else x::(dellist (n-1) xs)

(* 35 *)
fun countlist [] = 0
  | countlist (x::xs) = 1 + (countlist xs)

(* 36 *)
fun subst _ [] = []
  | subst (a,b) (x::xs) = 
      if x = a
        then b::(subst (a,b) xs)
        else x::(subst (a,b) xs)

(* 37 *)
fun revm [] [] = []
  | revm _ [] = raise Error
  | revm [] _ = raise Error
  | revm (x::xs) (y::ys) = x::(revm xs ys) @ [y] 

(* 38 *)
fun merge [] [] = []
  | merge [] rs = rs
  | merge ls [] = ls
  | merge (l::ls) (r::rs) = l::r::(merge ls rs)

(* 39 *)
fun merge2 [] [] = []
  | merge2 [] rs = raise LengthError
  | merge2 ls [] = raise LengthError
  | merge2 (l::ls) (r::rs) = l::r::(merge ls rs)

(* 40 *)
fun foldr f a [] = a
  | foldr f a (x::xs) = f (x, (foldr f a xs))

(* 41 *)
fun foldl f a [] = a
  | foldl f a (x::xs) = foldl f (f(x, a)) xs 

fun mem2 x y = member y x

(* 42 *)
val intersect = fn xs => fn ys => List.filter (mem2 ys) xs

(* 43 *)
val subtract = fn xs => fn ys => List.filter (not o (mem2 ys)) xs

(* 44 *)
val union = fn xs => fn ys => xs @ (subtract ys xs)

(* 45 *)
fun set_equal xs ys =  ((subtract xs ys) = []) andalso ((subtract ys xs) = [])

(* 46 *)
fun distinct [] = true
  | distinct (x::xs) = (not (mem2 xs x)) andalso (distinct xs)

(* 47 *)
fun unique [] = []
  | unique (x::xs) = 
      if member x xs
        then unique xs
        else x::(unique xs)

fun insList xs 0 []      = xs
  | insList [] _ _       = raise InsertListError
  | insList xs 0 (y::ys) = y::(insList xs 0 ys)
  | insList (x::xs) n ys = 
      if n < 0 
        then raise InsertListError
        else x::(insList xs (n-1) ys)
