
exception ParseFail of string
type Src = char list;
datatype 'a PResult = Ok of ('a*Src) | Fail of (string*Src)
type 'a Parser = Src -> 'a PResult

fun foldl1 _ nil = raise Empty
  | foldl1 f (x::xs) = foldl f x xs;

fun >>* ((f1:'a Parser),(f2:'b Parser)) (s:Src) = 
  case f1 s of
       Ok(v, next) =>
        (case f2 next of
             Ok(res, remain) => Ok(res, remain)
           | Fail(msg, remain) => Fail(msg, s))
     | Fail(msg, remain) =>Fail(msg, remain)

fun *>> ((f1:'a Parser),(f2:'b Parser)) (s:Src) = 
  case f1 s of
       Ok(v, next) => 
        (case f2 next of
             Ok(_, remain) => Ok(v, remain)
           | Fail(msg, remain) => Fail(msg, s))
     | Fail(msg, remain) =>Fail(msg, remain)

fun >>! ((f1:'a Parser),(f2:'b Parser)) (s:Src) = 
  case f1 s of
       Ok(v, next) => 
        (case f2 next of
             Ok(_, _) => Fail(">>!", next)
           | Fail(msg, remain) => Ok(v, next))
     | Fail(msg, remain) =>Fail(msg, remain)

fun || ((f1:'a Parser),(f2:'a Parser)) (s:Src) = 
  case f1 s of
       Ok(v, next) => Ok(v, next)
     | Fail(msg, next) => 
         (case f2 next of
               Ok(v, remain) => Ok(v, remain)
             | Fail(cmsg, remain) => Fail(msg ^ " || " ^ cmsg, remain))

fun ?? ((p:'a Parser),(v:'a)) (s:Src)= 
  case p s of
       Ok(va, re) => Ok(va, re)
     | Fail(_, re) => Ok(v, re)

infix 3 >>*
infix 3 *>>
infix 3 >>!
infix 2 ||
infix 4 ??

fun tuple ((p1:'a Parser), (p2:'b Parser)) (f:'a*'b -> 'c) (s:Src) = 
  case p1 s of
       Ok(v1, next) =>
        (case p2 next of
             Ok(v2, remain) => Ok(f (v1, v2), remain)
           | Fail(msg, remain) => Fail(msg, s))
     | Fail(msg, remain) =>Fail(msg, remain)



fun many_plus (p:'a Parser) (s:Src) = 
let 
  val remain = ref (nil:Src)
  fun result src =
      case p src of
           Ok(v, re) => v::result re
         | Fail(msg, re) => (remain := re; nil)
in
  case result s of
       nil => Fail("No match", s)
     | xs   => Ok(xs, ! remain)
end;

fun many_star (p:'a Parser) (s:Src) = 
let 
  val remain = ref (nil:Src)
  fun result src =
      case p src of
           Ok(v, re) => v::result re
         | Fail(msg, re) => (remain := re; nil)
in
  Ok(result s, !remain)
end;

fun until (p:'a Parser) (s:Src) = 
let
  val remain = ref (nil: Src)
  fun seeker nil = nil 
    | seeker (c::cs) = 
        case p (c::cs) of 
             Ok(v, re) => (remain := (c::cs); nil)
           | Fail(v, re) => c :: seeker cs
in
  Ok(implode (seeker s),! remain)
end;

fun convert (f:'a -> 'b) (p:'a Parser) (s:Src) = 
  case p s of
       Ok(v, re) => Ok(f v, re)
     | Fail(msg, re) => Fail(msg, re)

fun PAnyChar nil    = Fail("No char", nil)
  | PAnyChar (c::s) = Ok(c, s)

fun PChar (e:char) nil = Fail("Missing "^(str e), nil)
  | PChar (e:char) ((c::s):Src) = 
      if e = c 
        then Ok(e, s)
        else Fail("Missing "^(str e), (c::s))

fun PExpression (expr:string) (nil:Src) = Fail("Missing "^expr, nil)
  | PExpression expr s = 
  let 
    val char_parsers = map PChar (rev (explode expr))
    val parser = foldl1 op*>> char_parsers
  in
    case parser s of
         Ok(_, re) => Ok(expr, re)
       | Fail(_, re) => Fail("Missing "^expr, re)
  end

fun PLiteral ((expr:string), value:'a) (s:Src) =
  case PExpression expr s of
       Ok(_, remain) => Ok(value, remain)
     | Fail(msg, remain) => Fail(msg, remain)

val PWs = many_star ((PChar #" ") || (PChar #"\n") || (PChar #"\t"))

fun PDigit (nil:Src) = Fail("No digit", nil)
  | PDigit (c::s) = 
    let val n = ord(c) - ord(#"0")
    in if 0 <= n andalso n <= 9
       then Ok(n, s)
       else Fail("Not a digit", (c::s))
    end

fun PUInt (s:Src) = 
let
  val foldfunc = foldl (fn (x,y) => x + 10*y) 0
in
  case many_plus PDigit s of
       Ok(v, remain) => Ok(foldfunc v, remain)
     | Fail(msg, remain) => Fail(msg, remain)
end


fun PSign (s:Src) = 
    case PChar #"~" s of
         Ok(_, re) => Ok(~1, re)
       | Fail(_, re) => Ok(1, re)

fun PSign_R (s:Src) = 
    case PChar #"~" s of
         Ok(_, re) => Ok(~1.0, re)
       | Fail(_, re) => Ok(1.0, re)


val PInt = tuple (PSign, PUInt) op*

val PUReal = 
  let 
    fun dotfold (i, d) = real(i) +
      (real(d) * Math.pow(0.1, real(ceil(Math.log10(real(d))))))
    fun efold (v, e) = v * (Math.pow(10.0, real(e))) 
  val dotreal = (tuple (PUInt ?? 0 , PChar #"." >>* PUInt) dotfold) 
  in
    tuple (dotreal || convert real PUInt, ((PChar #"E" || PChar #"e") >>* PInt) ) efold
  end
val PReal = tuple (PSign_R, PUReal) op*;


fun PToken p = PWs >>* p *>> PWs

val PString = PChar #"\"" >>* until (PChar #"\"") *>> PChar #"\""

val PBracketDelim = PToken (PChar #",") 
fun PBracket p_elem = 
  PChar #"[" >>* (many_star (PToken (p_elem *>> PBracketDelim || p_elem))) *>> PChar #"]"

fun PList p_elem = PToken (PBracket p_elem || PLiteral ("nil", nil))

val PIntList = PList PInt
val PStringList = PList PString

val T = PLiteral ("True", true)
val F = PLiteral ("False", false)

fun parse (p: 'a Parser) (s:string) = 
  case p (explode s) of
       Ok(v, _) => v
     | Fail(msg, _) => raise ParseFail msg
