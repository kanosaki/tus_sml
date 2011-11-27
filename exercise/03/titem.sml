
signature TITEM = sig
  eqtype titem
  val isequal   : titem -> titem  -> bool
  val isless    : titem -> titem  -> bool
  val isgreater : titem -> titem  -> bool
  val show      : titem -> string
end

structure StringTItem : TITEM = struct
  type titem = string
  fun isequal (a:titem) b = a = b
  fun isless (a:titem) b = a < b
  fun isgreater (a:titem) b = a > b
  fun show (a:titem) = a 
end

structure IntTItem : TITEM = struct
  type titem = int
  fun isequal (a:titem) b = a = b 
  fun isless (a:titem) b = a < b
  fun isgreater (a:titem) b = a > b
  fun show (a:titem) = Int.toString(a)
end
structure CharTItem : TITEM = struct
  type titem = char
  fun isequal (a:titem) b = a = b
  fun isless (a:titem) b = a < b
  fun isgreater (a:titem) b = a > b
  fun show (a:titem) = Char.toString(a) 
end
