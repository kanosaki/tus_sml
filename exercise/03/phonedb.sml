
use "items.sml";
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
    fun add k v (PhoneDb db) = PhoneDb ((k, v) :: db)
    fun find t (PhoneDb nil) = raise NotFound
      | find t (PhoneDb ((k,v)::db)) =
          if k = t
            then v
            else find t (PhoneDb db)
    fun show (PhoneDb nil) = ()
      | show (PhoneDb ((k,v)::nil)) = 
          print ((KI.show k)^(" : ")^(VI.show v))
      | show (PhoneDb ((k,v)::others)) = 
        (
          print ((KI.show k)^(" : ")^(VI.show v));
          print ", ";
          show (PhoneDb others)
        )
  end
end

