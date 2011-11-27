
use "items.sml";
use "assoctable.sml";
exception NotFound
signature PHONE_DB = sig
  structure KI    : ITEM 
  structure VI    : ITEM
  type phonedb
  val empty      : phonedb
  val add        : KI.item -> VI.item  -> phonedb -> phonedb
  val find       : KI.item -> phonedb -> VI.item
  val show       : phonedb -> unit
end

functor MkPhoneDB (KItem : ITEM) (VItem : ITEM) : PHONE_DB = 
struct
  structure KI = KItem
  structure VI = VItem
  abstype phonedb = PhoneDb of (KI.item*VI.item) list with
    val empty = PhoneDb nil 
    fun add k v (PhoneDb db) = 
      PhoneDb ((k, v) :: (List.filter (fn (x,_) => not (KI.isequal k x)) db))
    fun find t (PhoneDb nil) = raise NotFound
      | find t (PhoneDb ((k,v)::db)) =
          if k = t
            then v
            else find t (PhoneDb db)
    fun show (PhoneDb nil) = ()
      | show (PhoneDb ((k,v)::nil)) = 
          print ((KI.show k)^(" : ")^(VI.show v)^"\n")
      | show (PhoneDb ((k,v)::others)) = 
        (
          print ((KI.show k)^(" : ")^(VI.show v));
          print ", ";
          show (PhoneDb others)
        )
  end
end

structure StrPDbItem : OrderdType = struct
  type key = string
  fun compare (a:string) b = 
    if a < b 
      then ~1 
      else (if a > b then 1 else 0)
end;

structure PhoneNumber : ITEM = struct
  type item = string
  fun isequal a b = a = b
  fun show s = s
end

structure Name : ITEM = struct
  type item = string
  fun isequal a b = a = b
  fun show s = s
end
structure Pdb = MkPhoneDB (Name) (PhoneNumber)
