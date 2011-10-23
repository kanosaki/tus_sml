
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

fun || ((f1:'a Parser),(f2:'a Parser)) (s:Src) = 
  case f1 s of
       Ok(v, next) => Ok(v, next)
     | Fail(msg, next) => 
         (case f2 next of
               Ok(v, remain) => Ok(v, remain)
             | Fail(cmsg, remain) => Fail(msg ^ " || " ^ cmsg, remain))

infix 3 >>*
infix 3 *>>
infix 2 ||

fun tuple ((p1:'a Parser), (p2:'b Parser)) (f:'a*'b -> 'c) (s:Src) = 
  case p1 s of
       Ok(v1, next) =>
        (case p2 next of
             Ok(v2, remain) => Ok(f (v1, v2), remain)
           | Fail(msg, remain) => Fail(msg, s))
     | Fail(msg, remain) =>Fail(msg, remain)

fun sequence (p:'a Parser) (f:'a list-> 'b) (s:Src) = 
let 
  val remain = ref (nil : Src)
  fun inner (Fail(_, re)) = (remain := re; nil)
    | inner (Ok(v, re)) = v :: inner (p re)
  val result = inner (p s)
in
  case result of
       nil => Fail("No element ", s)
     | _   => Ok(f result, !remain)
end;


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

fun PAnyChar nil    = Fail("No char", nil)
  | PAnyChar (c::s) = Ok(c, s)

fun PChar (e:char) nil = Fail("Missing "^(str e), nil)
  | PChar (e:char) ((c::s):Src) = 
      if e = c 
        then Ok(e, s)
        else Fail("Missing "^(str e), (c::s))

fun PString (expr:string) (nil:Src) = Fail("Missing "^expr, nil)
  | PString expr s = 
  let 
    val char_parsers = map PChar (rev (explode expr))
    val parser = foldl1 op*>> char_parsers
  in
    case parser s of
         Ok(_, re) => Ok(expr, re)
       | Fail(_, re) => Fail("Missing "^expr, re)
  end

fun PLiteral ((expr:string), value:'a) (s:Src) =
  case PString expr s of
       Ok(_, remain) => Ok(value, remain)
     | Fail(msg, remain) => Fail(msg, remain)

val PWs = many_star (PChar #" ")

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

fun PToken p = PWs >>* p *>> PWs

val PInt = tuple (PSign, PUInt) op*
val PBracketDelim = PWs >>* PChar #"," *>> PWs

fun PBracket (p_elem:'a Parser) = 
  PChar #"[" >>* (many_star (PToken (p_elem *>> PBracketDelim || p_elem))) *>> PChar #"]"

fun PList (p_elem:'a Parser) = PToken (PBracket p_elem || PLiteral ("nil", nil))

val PIntList = PList PInt

val T = PLiteral ("True", true)
val F = PLiteral ("False", false)

fun parse (p: 'a Parser) (s:string) = 
  case p (explode s) of
       Ok(v, _) => v
     | Fail(msg, _) => raise ParseFail msg
