
signature ITEM = sig
  eqtype item
  val isequal : item -> item   -> bool
  val show    : item -> string
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
