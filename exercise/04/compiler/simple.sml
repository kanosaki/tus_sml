
(*{{{ *) (* }}} *)
(* Lexer {{{ *)

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
 (* }}} *)

(* Ast {{{ *) 
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
(* }}} *)

(* Parser {{{ *) 
(* helper functions *)
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
       | (L.ONE "%")    =>
            (advance();
            let val c_factor = factor() in
              term' (A.App(A.Var("%"), A.Pair(prev_term, c_factor)))
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
       | Ast.NilDec => print "NilDec "
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
(* }}} *)

(* Table {{{ *) 
structure Table = struct
  structure A = Ast
  structure L = Lexer
  exception NoDeclaration
  val init  = fn x => raise NoDeclaration
  fun update v offset env = fn x => if v = x then offset else env x
  val varAddr = ref 0
  fun nextAddr () = (varAddr := (!varAddr)+1; !varAddr)

  val stack = ref 0
  fun stack_move n = (stack := ((!stack) + n))
  val maxStack = ref 0
  fun setMaxSize () = 
    if !stack > !maxStack 
    then (maxStack := !stack) 
    else ()
  fun stackSize (A.Def(_,e)) = 
        (stackSize_expr e; 
         stack_move ~1)
    | stackSize (A.If (e, s1, s2)) = 
        (stackSize_expr e; 
         stack_move ~1; 
         stackSize s1;
         case s2 of
              SOME s =>  stackSize s
            | NONE   => ())
    | stackSize (A.While (e,s)) = 
        (stackSize_expr e;
         stack_move ~1;
         stackSize s)
    | stackSize (A.Iprint e)    = 
        (stack_move 1;
         setMaxSize();
         stackSize_expr(e);
         stack_move ~2)
    | stackSize (A.Scan _) = 
        (stack_move 1;
         setMaxSize();
         stack_move ~1)
    | stackSize (A.Sprint e) = 
        (stack_move 1;
         setMaxSize();
         stackSize_expr e;
         stack_move ~2)
    | stackSize (A.Block (_, ss)) = app stackSize ss
    | stackSize _   = ()
  and stackSize_expr (A.Num _) = (stack_move 1; setMaxSize())
    | stackSize_expr (A.Var _) = (stack_move 1; setMaxSize())
    | stackSize_expr (A.String _) = (stack_move 1; setMaxSize())
    | stackSize_expr (A.App (_, e)) = 
        (stackSize_expr e; stack_move 1;setMaxSize())
    | stackSize_expr (A.Pair (e1, e2)) = 
        (stackSize_expr e1; stackSize_expr e2;stack_move ~2)

  fun localSize (A.Block (A.Dec l, ss)) = 
      let val n = List.foldl (fn (_,c) => c+1) 0 l in
        List.foldl (fn (s,c) => c + localSize s) n ss 
      end
    | localSize (A.If (_, s1, s2)) = 
        (localSize s1) + (case s2 of SOME s => localSize s | NONE => 0)
    | localSize (A.While (_, s))   = localSize s
    | localSize _   = 0

  fun reset () = (varAddr := 0; stack := 0; maxStack := 0)
end
(* }}} *)

(* Emitter {{{ *) 
structure Emitter = struct 
  structure A = Ast
  structure T = Table


  exception InternalError
  val ostream = ref TextIO.stdOut
  fun out str = TextIO.output(!ostream, str)
  fun out_m s = out ("\t"^s^"\n")
  fun out_label i = out ("L"^(Int.toString i)^":\n")
  fun out_proc ss = app out (map (fn s => "\t"^s^"\n") ss)
  fun out_goto i = out_m ("goto L"^(Int.toString i))
  fun lookup_var env s = Int.toString(env s)

  val label = ref 0
  fun incLabel () = (label := !label + 1; !label)

  fun emit_dec d env = 
    case d of
         A.Dec (sl)   => foldl (fn (s,e) => T.update s (T.nextAddr()) e) env sl
       | A.NilDec   => env
  and emit_stmt s env =
    case s of
         A.Def (s,e) => 
           (emit_expr e env;
            out_m ("istore "^(lookup_var env s)))
       | A.If (c,s, opt) => 
           let 
             val i = emit_expr c env
             val j = incLabel()  
           in
             emit_stmt s env;
             case opt of 
                  SOME s2 => 
                    (out_goto j;
                     out_label i;
                     emit_stmt s2 env;
                     out_label j)
                | NONE    => (out_label i)
           end
       | A.While (c,s)  => 
           let val i = incLabel() in
             out_label i;
             let val j = emit_expr c env in
               emit_stmt s env;
               out_goto i;
               out_label j
             end
           end
       | A.Sprint s     => 
           (out_m "getstatic java/lang/System/out Ljava/io/PrintStream;";
            emit_expr s env;
            out_m "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V")
       | A.Iprint e     =>
           (out_m "getstatic java/lang/System/out Ljava/io/PrintStream;";
            emit_expr e env;
            out_m "invokevirtual java/io/PrintStream/print(I)V")
       | A.Scan s       => 
           out_proc [
              "invokestatic Scan/scan()I",
              "istore "^(lookup_var env s)
           ]
       | A.NilStmt      => ()
       | A.Block (d,l)  => 
           let 
             val env' = emit_dec d env
           in 
             app (fn st => emit_stmt st env') l
           end
  and emit_expr ast env =
    case ast of
         A.App (e1, e2) =>
            (emit_expr e2 env;
             case e1 of
                  A.Var "+" => (out_m ("iadd");0)
                | A.Var "-" => (out_m ("isub");0)
                | A.Var "*" => (out_m ("imul");0)
                | A.Var "/" => (out_m ("idiv");0)
                | A.Var "!" => (out_m ("ineg");0)
                | A.Var "%" => (out_m ("irem");0)
                | A.Var "EQ" =>
                    let val n = incLabel() in
                      out_m ("if_icmpne L"^Int.toString(n)); n
                    end
                | A.Var "NEQ" => 
                    let val n = incLabel() in
                      out_m ("if_icmpeq L"^Int.toString(n)); n
                    end
                | A.Var "GT" =>  
                    let val n = incLabel() in
                      out_m ("if_icmple L"^Int.toString(n)); n
                    end
                | A.Var "LT" => 
                    let val n = incLabel() in
                      out_m ("if_icmpge L"^Int.toString(n)); n
                    end
                | A.Var "GE" => 
                    let val n = incLabel() in
                      out_m ("if_icmplt L"^Int.toString(n)); n
                    end
                | A.Var "LE" => 
                    let val n = incLabel() in
                      out_m ("if_icmpgt L"^Int.toString(n)); n
                    end
                | _          => raise InternalError)
      | A.Pair (e1, e2) => (emit_expr e1 env; emit_expr e2 env)
      | A.Var s => (out_m ("iload "^(Int.toString(env s)));0)
      | A.Num s => (out_m ("ldc "^(Int.toString s));0)
      | A.String s => (out_m ("ldc \""^s^"\""); 0)
  fun emit ast localSize stackSize = 
    (out (
    ".class synchronized Aout\n"^
    ".super java/lang/Object\n"^
    ".method <init>()V\n"^
    "\t.limit locals 1\n"^
    "\t.limit stack 1\n"^
    "\taload_0\n"^
    "\tinvokenonvirtual java/lang/Object.<init>()V\n"^
    "\treturn\n"^
    ".end method\n\n"^
    ".method public static main([Ljava/lang/String;)V\n"^
    "\t.limit locals "^(Int.toString localSize)^"\n"^
    "\t.limit stack "^(Int.toString stackSize)^"\n");
    emit_stmt ast T.init;
    out(
    "\treturn\n"^
    ".end method\n"))

  fun reset () = (label := 0)
end
(* }}} *)

fun test () = 
  (Emitter.ostream := TextIO.openOut "tmp.j";
   let val ast = Parser.parse() 
   in
     Table.stackSize ast;
     Emitter.emit ast (Table.localSize ast + 1) (!Table.maxStack);
     TextIO.closeOut (!Emitter.ostream);
     OS.Process.system "jasmin tmp.j"
   end)

fun err str = TextIO.output(TextIO.stdErr, str)
fun main (name, args) = 
let 
  fun exec () = 
    case args of
         nil => (err (name^": missing file name\n");
                 OS.Process.exit OS.Process.failure)
       | (hd::tl)   => 
           (Parser.istream := TextIO.openIn hd;
            Emitter.ostream := TextIO.openOut "tmp.j";
            let val ast = Parser.parse() 
            in
              Table.stackSize ast;
              Emitter.emit ast (Table.localSize ast + 1) (!Table.maxStack);
              TextIO.closeOut (!Emitter.ostream);
              OS.Process.system "jasmin tmp.j"
            end)
in
  exec(); OS.Process.success
end

fun debug () = 
  (Emitter.ostream := TextIO.stdOut;
     Table.reset; Emitter.reset;
   let val ast = Parser.parse() 
   in
     Table.stackSize ast;
     print "--- AST:\n";
     Parser.print_stmt ast;
     print "\n--- Java Bytecode:\n";
     Emitter.emit ast (Table.localSize ast + 1) (!Table.maxStack)
   end)
