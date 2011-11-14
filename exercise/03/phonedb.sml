
signature ENTRY = sig
  eqtype item
  val new    : string -> string -> item
  val name   : item   -> string
  val number : item   -> string
end

signature PHONE_DB = sig
  structure I    : ENTRY
  eqtype phonedb
  val empty      : phonedb
  val add        : I.item  -> phonedb -> phonedb
  val find       : I.item  -> phonedb -> bool
  val show       : phonedb -> string
end

structure Entry : ENTRY = struct
  abstype entry = Entry of (string*string) with
    fun new name number = Entry (name, number)
    fun name ent = (#1 ent)
    fun number ent = (#2 ent)
  end
end

