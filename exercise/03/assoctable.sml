exception NotFound

signature OrderdType = sig
  eqtype key
  val compare: key -> key -> int
end

signature ASC = sig
  structure O : OrderdType
  type 'a t
  val empty: 'a t
  val add: O.key -> 'a -> 'a t -> 'a t
  val find: O.key -> 'a t -> 'a
  val remove: O.key -> 'a t -> 'a t
end

functor MkListASC (Itemstruct : OrderdType) : ASC = struct
  structure O = Itemstruct
  abstype 'a t = Asc of (O.key*'a) list with
    val empty = Asc(nil)
    fun add k v (Asc(dst)) = Asc((k,v)::dst)
    fun find _ (Asc(nil)) = raise NotFound 
      | find k (Asc((ak,av)::other)) = 
          if (O.compare ak k) = 0 
            then av
            else find k (Asc(other))
    fun remove k (Asc(kvs)) = Asc(List.filter (fn (ki,vi) => not ((O.compare ki k) = 0)) kvs)
  end
end

structure IntOItem : OrderdType = struct
  type key = int
  fun compare a b = a - b
end;

structure ISAsc = MkListASC (IntOItem)
