
fun split x = 
  let
    fun split_iter (x::xs) left right = split_iter xs right (x::left)
      | split_iter []    left right = (left, right)
  in
    split_iter x [] []
  end

fun marge xs ys lt =
  let
    fun marge_iter out (left as x::xs) (right as y::ys) lt = 
          if lt(x, y)
            then marge_iter (x::out) xs right lt
            else marge_iter (y::out) left ys lt
      | marge_iter out (x::xs) [] lt = marge_iter (x::out) xs [] lt
      | marge_iter out [] (y::ys) lt = marge_iter (y::out) [] ys lt
      | marge_iter out [] [] _     = out
  in
    marge_iter [] xs ys lt
  end

fun msort (empty as []) _ = empty
  | msort (single as _::[]) _ = single
  | msort xs lt = 
  let
    val (l, r) = split xs
    val sl = msort l lt
    val sr = msort r lt
    val r = marge sl sr lt
  in
    rev r
  end

