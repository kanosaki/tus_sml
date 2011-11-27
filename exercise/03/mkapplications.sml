
use "titem.sml";

signature TREE = sig
  structure I : TITEM
  type tree
  val empty   : tree
  val isempty : tree    -> bool
  val cons    : I.titem -> tree -> tree -> tree
  val leaf    : I.titem -> tree
  val left    : tree    -> tree
  val right   : tree    -> tree
  val root    : tree    -> I.titem
  val show    : tree    -> char list
end

functor MkTree (Itemstruct : TITEM) : TREE = 
struct 
  structure I = Itemstruct
  abstype tree = Empty | Tree of tree * I.titem * tree with
    val empty = Empty
    fun isempty (Tree t) = false
      | isempty Empty = true
    fun cons i l r = Tree (l,i,r)
    fun leaf i = Tree (empty,i,empty)
    fun left (Tree (l,i,r)) = l
      | left Empty = hd nil
    fun right (Tree (l,i,r)) = r
      | right Empty = hd nil
    fun root (Tree (l,i,r)) = i
      | root Empty = hd nil
    fun show (Tree (l,i,r)) = (#"(")::(showleft l)@(explode (I.show i))@(showright r)@[#")"]
      | show Empty = [#"$"]
    and showleft (Tree (l,i,r)) = show (Tree (l,i,r))@[#","]
      | showleft Empty = [#"$",#","]
    and showright (Tree (l,i,r)) = (#",") :: (show (Tree (l,i,r))) 
      | showright Empty = [#",",#"$"]
  end 
end

functor MkApplications (T : TREE) = 
struct
  open T
  fun size t = 
        if isempty t 
          then 0 
          else 1 + (size (left t)) + (size (right t))
  fun height t = 
        if isempty t 
          then 0 
          else 
            (let 
              val rh = height (right t)
              val lh = height (left t)
            in
              1 + (if rh > lh then rh else lh)
            end)
end

structure ITree = MkTree (IntTItem)
structure IApp = MkApplications (ITree)
