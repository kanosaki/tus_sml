
structure Lexer = struct
  datatype token = INT 
                 | WHILE 
                 | IF | ELSE 
                 | EQ | GE | LE | NEQ 
                 | SCAN | SPRINT | IPRINT 
                 | ID of string 
                 | NUM of int 
                 | STRING of string 
                 | ONE of string 
                 | EOF


  fun read istream = 
    case TextIO.input1 istream of
         SOME x   => String.str x
       | NONE     => ""

  and integer istream i = 
  let val c = TextIO.lookahead istream 
  in
    case c of
         NONE     => 0
       | SOME v   => 
           (if (Char.isDigit v) 
              then integer istream 
                            (10 * i + ord(String.sub(read istream, 0)) - ord(#"0"))
              else i)
  end

  and identifier istream id = 
  let val c = TextIO.lookahead istream in
    case c of
         NONE     => ""
       | SOME v   => 
           if (Char.isLower v) orelse (Char.isUpper v)
              orelse (Char.isDigit v) orelse v = #"_"
             then identifier istream (id ^ (read istream))
             else id
  end
  and str istream s =
  let val c = TextIO.lookahead istream in
    case c of 
         NONE     => ""
       | SOME v   => 
           if (v <> #"\"")
            then str istream (s ^ (read istream))
            else s

  end
  and native_token istream = 
  let val c = TextIO.lookahead istream in
    case c of 
         NONE     => EOF
       | SOME v   => 
           if (Char.isLower v) orelse (Char.isUpper v)
            then
              let val id = identifier istream "" in
                case id of
                     "int"    => INT
                   | "if"     => IF
                   | "else"   => ELSE
                   | "while"  => WHILE
                   | "sprint" => SPRINT
                   | "iprint" => IPRINT
                   | "scan"   => SCAN
                   | _        => ID (id)
              end
           else if (Char.isDigit v) then NUM(integer istream 0)
           else if (v = #"\"") then 
             (read istream;
              let val s = str istream "" in
                read istream; STRING s
              end)
           else if (v = #"=" orelse v = #"!" orelse v = #"<" orelse v = #">") then
             (read istream;
             let val next = TextIO.lookahead istream in
               case next of
                    NONE      => ONE (String.str v)
                  | SOME nc   =>
                      (if nc = #"=" then (read istream;()) else (); 
                      case (v, nc) of
                           (#"=",#"=") => EQ
                         | (#"!",#"=") => NEQ
                         | (#">",#"=") => GE
                         | (#"<",#"=") => LE
                         | (_,_)       => ONE (String.str v))
             end)
           else ONE (read istream)
  end

  and gettoken istream =
  let val token = native_token istream in
    case token of 
         ONE " "  => gettoken istream
       | ONE "\t" => gettoken istream
       | ONE "\n" => gettoken istream
       | _        => token
  end

fun print_token (ID i)     = (print "ID("; print i; print ")")
  | print_token (NUM n)    = (print "NUM("; print (Int.toString n); print ")")
  | print_token (STRING s) = (print "STRING("; print s; print ")")
  | print_token (INT)      = print "INT"
  | print_token (WHILE)    = print "WHILE"
  | print_token (IF)       = print "IF"
  | print_token (ELSE)     = print "ELSE"
  | print_token (SCAN)     = print "SCAN"
  | print_token (SPRINT)   = print "SPRINT"
  | print_token (IPRINT)   = print "IPRINT"
  | print_token (EQ)       = print "EQ"
  | print_token (NEQ)      = print "NEQ"
  | print_token (LE)       = print "LE"
  | print_token (GE)       = print "GE"
  | print_token (EOF)      = print "EOF"
  | print_token (ONE c)    = (print "ONE("; print c; print ")")

  exception EndOfStream

  fun run () = 
  let val istream = TextIO.stdIn in
    while true do (
      TextIO.flushOut TextIO.stdOut;
      let val rlt = gettoken istream in
        case rlt of
             EOF  => raise EndOfStream
           | _    => (print_token rlt; print "\n")
    end)
  end
end

structure L = Lexer

val istream = ref TextIO.stdIn
fun getToken () = L.gettoken (!istream)
val tok = ref (L.ONE "")
fun advance () = (tok := getToken(); L.print_token (!tok))
exception SyntaxError
fun error () = raise SyntaxError
fun eat t = if (!tok = t) then advance() else error()
fun eatID () = 
  case !tok of
       (L.ID _)     => advance()
     | _            => error()
fun eatNUM () =
  case !tok of
       (L.NUM _)    => advance()
     | _            => error()
fun eatSTR () =
  case !tok of
       (L.STRING _)    => advance()
     | _            => error()

fun parse () = (advance(); stmt())
and dec () = 
  case !tok of
       L.INT    => (advance(); ids(); eat(L.ONE ";"))
     | _        => ()
and ids () = (eatID(); ids'())
and ids' () =
  case !tok of
       (L.ONE ",")    => (advance(); eatID(); ids'())
     | _            => ()
and stmts () = 
  case !tok of
       L.ONE "}"    => ()
     | _            => (stmt(); stmts())
and stmt ()=
  case !tok of
       L.ID str     => (advance(); eat(L.ONE "="); expr(); eat(L.ONE ";"))
     | L.IF         => 
        (advance(); 
        eat(L.ONE "("); cond(); eat(L.ONE")");
        stmt(); else_opt())
     | L.WHILE      => (advance();eat(L.ONE "("); cond(); eat(L.ONE")");stmt())
     | L.SPRINT     => (advance();eat(L.ONE "("); eatSTR();eat(L.ONE")");eat(L.ONE ";"))
     | L.IPRINT     => (advance();eat(L.ONE "("); expr();eat(L.ONE")");eat(L.ONE ";"))
     | L.SCAN       => (advance();eat(L.ONE "("); eatID();eat(L.ONE")");eat(L.ONE ";"))
     | (L.ONE "{")    => (advance();dec();stmts();eat(L.ONE "}"))
     | _            => ()
and else_opt () = 
  case !tok of
       L.ELSE   => (advance(); stmt())
     | _        => ()
and expr () = (term(); expr'())
and expr' () = 
  case !tok of
       (L.ONE "+")    => (advance(); term(); expr'())
     | (L.ONE "-")    => (advance(); term(); expr'())
     | _            => ()
and term () = (factor(); term'())
and term' () = 
  case !tok of
       (L.ONE "*")    => (advance(); factor(); term'())
     | (L.ONE "/")    => (advance(); factor(); term'())
     | _            => ()
and factor () = 
  case !tok of 
       L.ID str     => advance()
     | L.NUM num    => advance()
     | L.ONE "("    => (advance(); expr(); eat(L.ONE ")"))
     | L.ONE "-"    => (advance(); expr())
     | _            => error()
and cond () = (expr(); condop(); expr())
and condop () = 
  case !tok of
       L.EQ     => advance()
     | L.NEQ    => advance()
     | L.ONE ">" => advance()
     | L.ONE "<" => advance()
     | L.GE     => advance()
     | L.LE     => advance()
     | _        => error()


(* 12/21 13:00 *)
