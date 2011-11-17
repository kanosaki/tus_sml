
use "items.sml";
exception NotFound
signature PHONE_DB = sig
  structure KI    : ITEM 
  structure VI    : ITEM
  type phonedb
  val empty      : phonedb
  val add        : KI.item -> VI.item  -> phonedb -> phonedb
  val find       : KI.item -> phonedb -> VI.item
  val show       : phonedb -> string
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
    fun show db = "Not implemented"
  end
end

