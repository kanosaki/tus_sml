
abstype queue = Queue of string list with
  val qempty = Queue nil
  fun isqempty (Queue q) = q = nil
  fun enqueue i (Queue q) = Queue (q @ [i])
  fun dequeue (Queue nil) = raise Empty
    | dequeue (Queue (e::l)) = Queue l
  fun qtop (Queue nil) = raise Empty
    | qtop (Queue (e::l)) = e
  fun qlength (Queue q)= length q 
  fun show (Queue nil) = ()
    | show (Queue (t::nil)) = (print t; print "\n")
    | show (Queue (t::l)) = (print t; print ","; show (Queue l))
end
