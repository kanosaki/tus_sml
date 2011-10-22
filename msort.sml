
(* Merge sort sample *)


fun split x = 
  let 
    fun split_iter (x::xs, left, right) = split_iter(xs, right, x::left)
    |   split_iter ([], left, right)    = (left, right)
  in
    split_iter(x, [], [])
  end

fun merge (x, y, lt) = 
  let
    fun merge_iter (out, left as (x::xs), right as (y::ys), lt) = 
      if lt(x, y)
        then merge_iter(x::out, xs, right, lt)
        else merge_iter(y::out, left, ys, lt)
      | merge_iter (out, x::xs, [], lt) = merge_iter(x::out, xs, [], lt)
      | merge_iter (out, [], y::ys, lt) = merge_iter(y::out, [], ys, lt)
      | merge_iter (out, [], [], _) = out
  in
    merge_iter([], x, y, lt)
  end

fun mergesort(empty as [], _) = empty
  | mergesort(single as _::[], _) = single
  | mergesort(x, lt) = 
    let 
      val (left, right) = split(x)
      val sl = mergesort(left, lt)
      val sr = mergesort(right, lt)
      val s = merge(sl, sr, lt)
    in
      rev s
    end
