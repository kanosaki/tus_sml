exception NotFound
exception Error

signature OrderdType = sig
  eqtype key
  val compare: key -> key -> int
end

signature ASC = sig
  structure O : OrderdType
  type 'a table
  val empty  : 'a table
  val add    : O.key    -> 'a       -> 'a table -> 'a table
  val find   : O.key    -> 'a table -> 'a
  val remove : O.key    -> 'a table -> 'a table
end

functor MkListASC (Itemstruct : OrderdType) : ASC = struct
  structure O = Itemstruct
  abstype 'a table = Asc of (O.key*'a) list with
    val empty = Asc(nil)
    fun add k v (Asc(dst)) = 
      Asc((k,v)::(List.filter (fn (x,_) => not (k = x)) dst))
    fun find _ (Asc(nil)) = raise NotFound 
      | find k (Asc((ak,av)::other)) = 
          if (O.compare ak k) = 0 
            then av
            else find k (Asc(other))
    fun remove k (Asc(kvs)) = Asc(List.filter (fn (ki,vi) => not ((O.compare ki k) = 0)) kvs)
  end
end

functor MkTreeASC (Itemstruct : OrderdType) = 
struct
  structure O = Itemstruct
  abstype 'a table = Leaf | Node of 'a table * (O.key*'a) * 'a table with

    val empty = Leaf

    fun add k v Leaf = Node(Leaf, (k,v), Leaf)
      | add k v (Node(l,(tk,tv),r)) = 
        if O.compare k tk < 0
          then (Node(add k v l,(tk, tv),r))
          else (if O.compare k tk > 0
                    then  (Node(l, (tk,tv), add k v r))
                    else  (Node(l, (tk,v), r)))

    fun find k Leaf = raise NotFound
      | find k (Node(l,(tk,tv),r)) = 
        if O.compare tk k = 0
          then tv
          else (if O.compare k tk < 0
                  then find k l
                  else find k r)
    fun cut_node Leaf = Leaf
      | cut_node (Node(Leaf,_,Leaf)) = Leaf
      | cut_node (Node(Leaf,_,r)) = r
      | cut_node (Node(l,_,Leaf)) = l
      | cut_node (Node(l,v,r)) = 
        let 
          fun cut_max Leaf = Leaf
            | cut_max (Node(lh,_,Leaf)) = lh
            | cut_max (Node(cl,cv,Node(nl,nv,Leaf))) = Node(cl,cv,nl)
            | cut_max (Node(cl,cv,cr)) = Node(cl,cv, cut_max cr)
          fun max_node Leaf = raise NotFound
            | max_node (Node(l,v, Leaf)) = Node(l,v,Leaf)
            | max_node (Node(_,_,r)) = max_node r
        in
          case max_node l of
               Leaf => raise Error
             | Node(_,v,_) => Node(cut_max l, v, r)
        end

    fun remove k Leaf = raise NotFound
      | remove k (Node(l,(tk,tv),r)) = 
        if O.compare tk k = 0
          then cut_node (Node(l,(tk,tv),r))
          else (if O.compare k tk < 0
                  then Node(remove k l, (tk, tv), r)
                  else Node(l, (tk, tv), remove k r))
  end
end

structure IntOItem : OrderdType = struct
  type key = int
  fun compare a b = a - b
end;

structure ISAsc = MkListASC (IntOItem)
fun foldl_i _ nil = raise Empty 
  | foldl_i f (x::xs) = foldl f x xs; 
fun init_table xs = (foldl_i (op o)  (map (fn (k,v) => ISAsc.add k v) xs)) ISAsc.empty;

structure Asc = MkTreeASC (IntOItem)
fun init_table xs = (foldl_i (op o)  (map (fn (k,v) => Asc.add k v) xs)) Asc.empty;
