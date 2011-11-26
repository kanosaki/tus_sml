abstype vector = Vector of int list with
  val vempty = Vector nil
  exception IndexError
  fun isvempty (Vector v) = v = nil
  fun vector l = Vector l
  fun at i (Vector v) = List.nth (v, i - 1) 
    handle Subscript => raise Empty
  fun vlength (Vector v) = length v
  fun vshow (Vector nil) = ()
    | vshow (Vector (t::nil)) = 
        (print (Int.toString t); 
        print "\n")
    | vshow (Vector (t::l)) = 
        (print (Int.toString t);
         print ","; 
         vshow (Vector l))
end
