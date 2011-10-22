
exception ParseFail
type Src = char list;
datatype 'a PResult = Ok of ('a*Src) | Fail of (string*Src)
type 'a Parser = Src -> 'a PResult

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

fun sequence (p:'a Parser) (f:'a list-> 'b) (s:Src) = 
let 
  val remain = nil : Src
  fun inner (Fail(_, re)) r = (r := re; nil)
    | inner (Ok(v, re)) r = v :: inner (p re) r
  val result = inner (p s) (ref remain)
in
  case result of
       nil => Fail("No element ", s)
     | _   => Ok(f result, remain)
end;


fun many_plus (p:'a Parser) (s:Src) = 
let 
  fun result src =
      case p src of
           Ok(v, re) => (v::result re)
         | Fail(msg, re) => nil
  fun remain src = 
      case p src of
           Ok(v, re) => remain re
         | Fail(msg, re) => re
  val res = result s
  val r = remain s
in
  case res of
       nil => Fail("No match", s)
     | _   => Ok(res, r)
end;

fun many_star (p:'a Parser) (s:Src) = 
let 
  fun result src =
      case p src of
           Ok(v, re) => (v::result re)
         | Fail(msg, re) => nil
  fun remain src = 
      case p src of
           Ok(v, re) => remain re
         | Fail(msg, re) => re
in
  Ok(result s, remain s)
end;

fun PAnyChar nil    = Fail("No char", nil)
  | PAnyChar (c::s) = Ok(c, s)

fun PChar (e:char) nil = Fail("Missing "^(str e), nil)
  | PChar (e:char) ((c::s):Src) = 
      if e = c 
        then Ok(e, s)
        else Fail("Missing "^(str e), (c::s))


val PString = sequence PAnyChar implode;

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

fun PInt (s:Src) = 
let val src = 
    case PChar #"~" s of
         Ok(_, remain) => (~1, remain)
       | Fail(_, remain) => (1, remain)
  val sign = #1 src
  val body = #2 src
in
  case PUInt body of
       Ok(v, r) => Ok(sign * v, r)
     | Fail(msg, r) => Fail(msg, r)
end

val PNumber = PWs >>* PInt *>> PWs

fun PBracket (b:char, e:char) (p_elem:'a Parser) = 
  PChar b >>* (many_star ((p_elem *>> PChar #",") || p_elem)) *>> PChar e

fun PList (p_elem:'a Parser) = 
  PWs >>* (PBracket (#"[", #"]") p_elem || PLiteral ("nil", nil)) *>> PWs

val T = PLiteral ("True", true)
val F = PLiteral ("False", false)

fun parse (p: 'a Parser) (s:string) = p (explode s)
