
signature FUNC = 
sig
  type func
  val f : int -> int 
end

functor MkMap ( ApplyFunc : FUNC ) = 
struct
  structure F = ApplyFunc
  abstype amap = Map with

end
end

