signature QUEUE =
sig
  type 'a queue
  exception Queue
  val empty     : 'a queue
  val isEmpty   : 'a queue -> bool
  val singleton : 'a -> 'a queue
  val insert    : 'a * 'a queue -> 'a queue
  val peek      : 'a queue -> 'a
  val remove    : 'a queue -> 'a * 'a queue
end

structure TwoListQueue :> QUEUE = 
struct
  type 'a queue = 'a list * 'a list
  exception Queue
  
  val empty = ([], [])
  
  fun isEmpty ([], []) = true
    | isEmpty _        = false

  fun singleton a = ([a], [])

  fun insert (a, (ins, outs)) = (a::ins, outs)

  fun peek ([], []) = raise Queue
    | peek (ins, []) = hd (rev ins)
    | peek (ins, a::outs) = a

  fun remove ([], []) = raise Queue
    | remove (ins, []) = 
      let 
        val newouts = rev ins
      in
        (hd newouts, ([], tl newouts))
      end
    | remove (ins, a::outs) = (a, (ins, outs))

end

functor BFT (Q: QUEUE) = 
struct
  datatype 'a tree
    = E
    | T of 'a * 'a tree * 'a tree
  
  fun bftQ (q : 'a tree queue) : 'a list = 
    if Q.isEmpty q then []
    else 
      let
        val (t, q') = Q.remove q
           in case t
            of E => bftQ q'
             | T (x, l, r) => 
                let
                  val q'' = Q.insert (r, Q.insert (l, q'))
                in
                  x :: bftQ q''
                end
      end
  
  fun bft t = bftQ (Q.singleton t)
end



