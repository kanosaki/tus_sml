

fun middle pre mid post = 
let 
  val pr = pre()
  val m = mid()
  val po = post()
in m
end

fun follows t fw = 
let
  val ret = t()
  fun inner nil     = ()
    | inner (f::fs) = (f(); inner fs)
in
  (inner fw; ret)
end

fun lazy f (u:unit) = f
fun eval_three a b c = (a(), b(), c())

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


structure Ast = struct
  datatype dec = Dec of string list | NilDec
  and     stmt = Def of string * expr 
               | If of expr * stmt * stmt option
               | While of expr * stmt 
               | Iprint of expr | Sprint of expr
               | Scan of string 
               | Block of dec * stmt list 
               | NilStmt
  and     expr = Num of int 
               | Var of string
               | String of string
               | App of expr * expr
               | Pair of expr * expr
end
structure Parser = struct
  structure L = Lexer
  structure A = Ast

  val istream = ref TextIO.stdIn
  fun getToken () = L.gettoken (!istream)
  val tok = ref (L.ONE "")
  fun advance () = (tok := getToken())
  exception SyntaxError
  fun error () = raise SyntaxError
  fun eat t = if (!tok = t) then advance() else error()
  fun eat_lazy f (_:unit) = eat f
  fun eatID () = 
    case !tok of
         (L.ID s)     => (advance(); s)
       | _            => error()
  fun eatNUM () =
    case !tok of
         (L.NUM n)    => (advance(); (A.Num n))
       | _            => error()
  fun eatSTR () =
    case !tok of
         (L.STRING s)    => (advance(); (A.String s))
       | _            => error()

  fun parse () = (advance(); stmt())
  and dec () = 
    case !tok of
         L.INT    => A.Dec(middle advance ids (eat_lazy(L.ONE ";")))
       | _        => A.NilDec
  and ids () = (eatID())::(ids'())
  and ids' () =
    case !tok of
         (L.ONE ",")    => (advance(); (eatID()) :: (ids'()))
       | _              => nil
  and stmts () = 
    case !tok of
         L.ONE "}"    => nil
       | _            => (stmt()) :: (stmts())
  and stmt ()=
    case !tok of
         L.ID str     => 
           (advance(); 
           A.Def(str, middle (eat_lazy(L.ONE "=")) expr (eat_lazy(L.ONE ";"))))
       | L.IF         => 
           (advance(); 
           let val cnd = middle (eat_lazy(L.ONE "(")) cond (eat_lazy(L.ONE")")) in
             A.If(cnd, stmt(), else_opt())
           end)
       | L.WHILE      => 
           (advance();
           let val cnd = middle (eat_lazy(L.ONE "(")) cond (eat_lazy(L.ONE")")) in
             A.While(cnd, stmt())
           end)
       | L.SPRINT     => 
           (advance(); eat(L.ONE "("); 
           A.Sprint(follows eatSTR [eat_lazy(L.ONE")"), eat_lazy(L.ONE ";")]))
       | L.IPRINT     => (advance();eat(L.ONE "(");
           A.Iprint(follows expr [eat_lazy(L.ONE")"), eat_lazy(L.ONE ";")]))
       | L.SCAN       => (advance();eat(L.ONE "(");
           A.Scan(follows eatID [eat_lazy(L.ONE")"), eat_lazy(L.ONE ";")]))
       | (L.ONE "{")    => (advance();
           case eval_three dec stmts (eat_lazy(L.ONE "}")) of
                (d,s,_)   => A.Block(d, s))
       | _            => A.NilStmt
  and else_opt () = 
    case !tok of
         L.ELSE   => (advance(); SOME (stmt()))
       | _        => NONE
  (*
  * expr  -> term expr'
  * expr' -> "+" term expr'
  * expr' -> "-" term expr'
  * expr' -> 
  * *)
  and expr () = 
    let val first_term = term() in
      expr' first_term
    end
  and expr' prev_expr = 
    case !tok of
         (L.ONE "+")    =>
            (advance();
            let val c_term = term() in
              expr' (A.App(A.Var("+"), A.Pair(prev_expr, c_term)))
            end)
       | (L.ONE "-")    => 
            (advance();
            let val c_term = term() in
              expr' (A.App(A.Var("-"), A.Pair(prev_expr, c_term)))
            end)
       | _            => prev_expr

  and term () = 
    let val first_factor = factor() in
      term' first_factor 
    end
  and term' prev_term = 
    case !tok of
         (L.ONE "*")    => 
            (advance();
            let val c_factor = factor() in
              term' (A.App(A.Var("*"), A.Pair(prev_term, c_factor)))
            end)
       | (L.ONE "/")    =>
            (advance();
            let val c_factor = factor() in
              term' (A.App(A.Var("/"), A.Pair(prev_term, c_factor)))
            end)
       | _            => prev_term
  and factor () = 
    case !tok of 
         L.ID str     => (advance(); A.Var(str))
       | L.NUM num    => (advance(); A.Num(num))
       | L.ONE "("    => middle advance expr (eat_lazy(L.ONE ")"))
       | L.ONE "-"    => (advance(); expr())
       | _            => error()
  and cond () = 
    case eval_three expr condop expr of
         (e1,cop,e2)  => A.App(cop, A.Pair(e1,e2))
  and condop () = 
    case !tok of
         L.EQ      => (advance(); A.Var("EQ"))
       | L.NEQ     => (advance(); A.Var("NEQ"))
       | L.ONE ">" => (advance(); A.Var("GT"))
       | L.ONE "<" => (advance(); A.Var("LT"))
       | L.GE      => (advance(); A.Var("GE"))
       | L.LE      => (advance(); A.Var("LE"))
       | _         => error()
  fun print_dec d =
    case d of 
         Ast.Dec sl => 
            (print "Dec[ ";
            map (fn s => (print s; print " ")) sl;print "]") 
       | Ast.NilDec => print "NilDec"
  and print_stmt s = 
    case s of
         A.Def (s,e) => (print "Def("; print s; print ","; print_expr e; print")")
       | A.If (c,s,opt) =>
           (print "If("; print_expr c; print ","; print_stmt s; print ",";
            case opt of 
                 SOME s2 => print_stmt s2 
               | NONE => () ; print ")")
       | A.While (c,s) => (print "While("; print_expr c; print ","; print_stmt s; print ")")
       | A.Sprint s => (print "Sprint("; print_expr s; print ")")
       | A.Iprint i => (print "Iprint("; print_expr i; print ")")
       | A.Scan s => (print "Scan("; print s; print")")
       | A.NilStmt => print "NilStmt"
       | A.Block (d,sl) => 
           (print "Block[ "; print_dec d;
            map (fn x => (print_stmt x; print " ")) sl; print "]")
  and print_expr x = 
    case x of 
         A.Var s => (print "Var "; print s)
       | A.App(e1,e2) => (print "App("; print_expr e1; print ","; print_expr e2; print ")")
       | A.Pair(e1,e2) => (print "Pair("; print_expr e1; print ","; print_expr e2; print ")")
       | A.Num n => (print "Num "; print (Int.toString(n)))
       | A.String s => (print "String \""; print s; print "\"")

end

(* 12/21 13:00 *)
