
abstype stack = Stack of int list with
  val empty = Stack nil
  fun isempty (Stack s) = s = nil
  fun push i (Stack s) = (Stack (i::s))
  fun top (Stack nil) = raise Empty
    | top (Stack (t::l)) = t
  fun pop (Stack nil) = raise Empty
    | pop (Stack (t::l)) = (Stack l)
  fun showstack (Stack nil) = ()
    | showstack (Stack (t::nil)) = 
        (print (Int.toString t) 
        print "\n")
    | showstack (Stack (t::l)) = 
        (print (Int.toString t) 
         print "," 
         showstack (Stack l))
end
  
