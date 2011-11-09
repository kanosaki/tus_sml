
val stack = nil;
fun isempty stack = stack = nil;
fun push i stack = (i::stack);
fun top nil = hd nil
  | top (t::l) = t;
fun pop nil = tl nil
  | pop (t::l) = l;
fun showstack nil = ()
  | showstack (t::nil) = (print (Int.toString t); print "\n")
  | showstack (t::l) = (print (Int.toString t); print ","; showstack l)
