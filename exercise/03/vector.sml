abstype vector = Vector of int list with
  val vempty = Vector nil
  exception IndexError
  fun isvempty (Vector v) = v = nil
  fun vector l = Vector l
  fun at i (Vector v) = let 
    fun inner 0 (x::xs) = x
      | inner n nil     = raise IndexError
      | inner n (x::xs) = inner (n-1) xs
  in 
    if i < 0 then raise IndexError
    else inner i v
  end
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
