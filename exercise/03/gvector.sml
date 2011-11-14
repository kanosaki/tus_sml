
signature ITEM = sig
  eqtype item
  val isequal : item -> item   -> bool
  val show    : item -> string
end

signature VECTOR = sig
  structure I : ITEM
  type vector
  val empty   : vector
  val isempty : vector      -> bool
  val vector  : I.item list -> vector
  val at      : int         -> vector -> I.item
  val length  : vector      -> int
  val show    : vector      -> unit
end

structure StringItem : ITEM = struct
  type item              = string
  fun isequal (a:item) b = a = b
  fun show (a:item)      = a
end

structure IntItem : ITEM = struct
  type item              = int
  fun isequal (a:item) b = a = b
  fun show (a:item)      = Int.toString a
end

structure StringItem : ITEM = struct
  type item              = char
  fun isequal (a:item) b = a = b
  fun show (a:item)      = Char.toString a
end

functor MkVector (Itemstruct : ITEM) : VECTOR = struct
  structure I = Itemstruct
  abstype vector = Vector of I.item list with
    val empty = Vector nil
    exception IndexError
    fun isempty (Vector v) = v = nil
    fun vector l = Vector l
    fun at i (Vector v) = List.nth (v, i)
    fun length (Vector v) = List.length v
    fun show (Vector nil) = ()
      | show (Vector (t::nil)) = 
          (print (I.show t); 
          print "\n")
      | show (Vector (t::l)) = 
          (print (I.show t);
           print ","; 
           show (Vector l))
  end
end



