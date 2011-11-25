
use "items.sml"; 

signature QUEUE = sig
  structure I : ITEM
  type queue
  val empty   : queue
  val isempty : queue   -> bool
  val enqueue : I.item  -> queue  -> queue
  val top     : queue   -> I.item
  val dequeue : queue   -> queue
  val length  : queue   -> int
  val show    : queue   -> unit
end

functor MkQueue (Itemstruct : ITEM) : QUEUE = struct
  structure I = Itemstruct
  abstype queue = Queue of I.item list with
    val empty = Queue nil
    fun isempty (Queue q) = q = nil
    fun enqueue i (Queue q) = Queue (q @ [i])
    fun dequeue (Queue nil) = raise Empty
      | dequeue (Queue (e::l)) = Queue l
    fun top (Queue nil) = raise Empty
      | top (Queue (e::l)) = e
    fun length (Queue q)= List.length q 
    fun show (Queue nil) = ()
      | show (Queue (t::nil)) = (print (I.show t); print "\n")
      | show (Queue (t::l)) = (print (I.show t); print ","; show (Queue l))
  end
end


structure StringItem = MkQueue(StringItem);
