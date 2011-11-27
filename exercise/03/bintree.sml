
use "titem.sml";

exception NotFound
exception Error

signature OTREE = sig
  structure I : TITEM
  type tree
  val empty : tree
  val isempty : tree -> bool
  val leaf : I.titem -> tree
  val left : tree -> tree
  val right : tree -> tree
  val root : tree -> I.titem
  val show : tree -> char list

  val insert : I.titem -> tree -> tree
  val remove : I.titem -> tree -> tree
  val max : tree -> I.titem
  val min : tree -> I.titem

  val trace : tree -> unit
end

functor MkTree (Itemstruct : TITEM) : OTREE = 
struct 
  structure I = Itemstruct
  exception TreeEmpty
  abstype tree = Empty | Tree of tree * I.titem * tree with
    val empty = Empty
    fun isempty (Tree t) = false
      | isempty Empty = true
    fun leaf i = Tree (empty,i,empty)
    fun left (Tree (l,i,r)) = l
      | left Empty = hd nil
    fun right (Tree (l,i,r)) = r
      | right Empty = raise TreeEmpty
    fun root (Tree (l,i,r)) = i
      | root Empty = raise TreeEmpty
    fun show (Tree (l,i,r)) = (#"(")::(showleft l)@(explode (I.show i))@(showright r)@[#")"]
      | show Empty = [#"$"]
    and showleft (Tree (l,i,r)) = show (Tree (l,i,r))@[#","]
      | showleft Empty = [#"$",#","]
    and showright (Tree (l,i,r)) = (#",") :: (show (Tree (l,i,r))) 
      | showright Empty = [#",",#"$"]

    fun insert i Empty = Tree(Empty, i,Empty)
      | insert i (Tree(l,v,r)) = 
        if I.isless i v
          then (Tree(insert i l, v, r))
          else (if I.isgreater i v
                  then (Tree(l,v,insert i r))
                  else (Tree(l, v, r)))

    fun max Empty = raise NotFound
      | max (Tree(_,v, Empty)) = v
      | max (Tree(_,_,r)) = max r

    fun min Empty = raise NotFound
      | min (Tree(Empty,v,_)) = v
      | min (Tree(l,_,_)) = min l

    fun cut_node Empty = Empty
      | cut_node (Tree(Empty,_,Empty)) = Empty
      | cut_node (Tree(Empty,_,r)) = r
      | cut_node (Tree(l,_,Empty)) = l
      | cut_node (Tree(l,v,r)) = 
        let 
          fun cut_max Empty = raise Error
            | cut_max (Tree(cl,cv,Tree(nl,nv,Empty))) = Tree(cl,cv,nl)
            | cut_max (Tree(cl,cv,cr)) = Tree(cl,cv, cut_max cr)
          fun max_node Empty = raise NotFound
            | max_node (Tree(l,v, Empty)) = Tree(l,v,Empty)
            | max_node (Tree(_,_,r)) = max_node r
        in
          case max_node l of
               Empty => raise Error
             | Tree(_,v,_) => Tree(cut_max l, v, r)
        end

    fun remove i Empty = raise NotFound
      | remove i (Tree(l,v,r)) = 
          if I.isequal v i 
            then cut_node ((Tree(l,v,r)))
            else (if I.isless i v 
                    then Tree(remove i l, v, r)
                    else Tree(l, v, remove i r))

    fun trace t = print ((implode (show t))^"\n")
  end 
end


structure IntTree = MkTree (IntTItem);
