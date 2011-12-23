
fun $ (x,y) = x y
infixr 0 $

(*{{{ *) (* }}} *)
(* Lexer {{{ *)
structure Lexer = struct
  datatype token = INT 
                 | WHILE | DO | FOR
                 | IF | ELSE 
                 | EQ | GE | LE | NEQ 
                 | SCAN | SPRINT | IPRINT 
                 | ID of string 
                 | NUM of int 
                 | STRING of string 
                 | ONE of string 
                 | EOF
  datatype pack = Pack of token * int

  val current_line = ref 0

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
                   | "for"    => FOR
                   | "do"     => DO
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
           | ONE "\n" => (current_line := !current_line + 1; gettoken istream)
           | _        => token
      end
  and getpack istream = 
      let val tok = gettoken istream in
        (tok, !current_line)
      end

  fun inspect (ID i)     = ("ID(" ^ i ^  ")")
    | inspect (NUM n)    = ("NUM("^ (Int.toString n)^ ")")
    | inspect (STRING s) = ("STRING("^ s^ ")")
    | inspect (INT)      = "INT"
    | inspect (WHILE)    = "WHILE"
    | inspect (IF)       = "IF"
    | inspect (ELSE)     = "ELSE"
    | inspect (SCAN)     = "SCAN"
    | inspect (SPRINT)   = "SPRINT"
    | inspect (IPRINT)   = "IPRINT"
    | inspect (EQ)       = "EQ"
    | inspect (NEQ)      = "NEQ"
    | inspect (LE)       = "LE"
    | inspect (GE)       = "GE"
    | inspect (EOF)      = "EOF"
    | inspect (DO)       = "DO"
    | inspect (FOR)      = "FOR"
    | inspect (ONE c)    = ("ONE("^ c^ ")")

  fun print_token t = print $ inspect t

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

(* Source {{{ *) 
structure Source = struct
  structure L = Lexer
  exception Error
  (* source stream , pre tokens , post tokens(usually nil) *)
  datatype source = Src of TextIO.instream * (L.token*int) list * (L.token*int) list

  fun next (Src(is, pre, po::posts)) = (po, Src(is, po::pre, posts))
    | next (Src(is, pre, nil)) = 
      let val tok = L.getpack is in
          (tok, Src(is, tok::pre, nil))
      end
  fun advance s = 
        case next s of
             (_, nex) => nex
  fun getCurrent (Src(_,p::pre,_)) = p
    | getCurrent _ = raise Error

  fun back (Src(is, nil, post)) = raise Error 
    | back (Src(is, (p::pre), post)) = Src(is, pre, p::post)
  fun init is = Src(is, nil, nil)

end
(* }}} *)

(* Ast {{{ *) 
structure Ast = struct
  datatype dec = Dec of string list | NilDec
  and     stmt = Def of string * expr 
               | If of expr * stmt * stmt option
               | While of expr * stmt 
               | For of string * int * int * stmt
               | Do of stmt * expr
               | Iprint of expr | Sprint of expr
               | Scan of string 
               | Block of dec * stmt list 
               | NilStmt
  and     expr = Num of int 
               | Var of string
               | Inc of string
               | String of string
               | App of expr * expr
               | Neg of expr
               | Pair of expr * expr

  fun inspect_dec d =
    case d of 
         Dec sl => 
            ( "Dec[ "^
            (foldl (fn (s,p) => (p^s^" ")) "" sl) ^ "]") 
       | NilDec =>  "NilDec "
  and inspect_stmt s = 
    case s of
         Def (s,e) => ( "Def("^s^","^inspect_expr e^")")
       | If (c,s,opt) =>
           ("If("^ inspect_expr c^","^inspect_stmt s^","^
            (case opt of 
                 SOME s2 => inspect_stmt s2 
               | NONE =>  ")"))
       | While (c,s) => ( "While("^inspect_expr c^","^ inspect_stmt s^")")
       | Sprint s => ( "Sprint("^inspect_expr s^")")
       | Iprint i => ( "Iprint("^inspect_expr i^")")
       | Scan s => ( "Scan("^s^")")
       | Do(s,e) => 
           ( "Do["^inspect_stmt s^ 
             "] while( "^inspect_expr e^" )")
       | For(v,b,e,st) => 
         ( ("for("^v^","^Int.toString(b)^","^Int.toString(e)^") ")^
          inspect_stmt st)
       | NilStmt =>  "NilStmt"
       | Block (d,sl) => 
           ( "Block[ "^ inspect_dec d^
            (foldl (fn (s,p) => (p^inspect_stmt s^" ")) "" sl)^  "]")
  and inspect_expr x = 
    case x of 
         Var s => ( "Var "^  s)
       | App(e1,e2) => ( "App("^ inspect_expr e1^  ","^ inspect_expr e2^  ")")
       | Pair(e1,e2) => ( "Pair("^ inspect_expr e1^  ","^ inspect_expr e2^  ")")
       | Num n => ( "Num "^  (Int.toString(n)))
       | String s => ( "String \""^  s^  "\" ")
       | Neg e    => ( "Neg["^ inspect_expr e^  "] ")
       | Inc s    => ( s^  "++ ")
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
  fun error msg  = (print "SYNTAX ERROR:"; print msg; print "\n" ;raise SyntaxError)
  fun eat t = 
    if (!tok = t) 
      then advance() 
      else (print ("SYNTAX ERROR: Requires ");(Lexer.print_token t);error "")
  fun eat_lazy f (_:unit) = eat f
  fun eatID () = 
    case !tok of
         (L.ID s)     => (advance(); s)
       | _            => error "ID required."
  fun eatNUM () =
    case !tok of
         (L.NUM n)    => (advance(); (A.Num n))
       | _            => error "Number requried."
  fun eatSTR () =
    case !tok of
         (L.STRING s)    => (advance(); (A.String s))
       | _            => error "String required."

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
           case !tok of
                (L.ONE "=") =>
                   A.Def(str, middle (eat_lazy(L.ONE "=")) expr (eat_lazy(L.ONE ";")))
              | (L.ONE "+") =>
                  (advance();
                  case !tok of
                       (L.ONE "+") => 
                          (advance();eat(L.ONE ";");
                           A.Def(str, A.App(A.Var("+"), A.Pair(A.Var(str), A.Num(1)))))
                     | (L.ONE "=") =>
                         (advance();
                         let val r_exp = expr() in
                           (eat(L.ONE ";"); 
                           A.Def(str, A.App(A.Var("+"), A.Pair(A.Var(str), r_exp))))
                         end)
                     | _ => error "Requires \"+ (x++; statement ) or \"= (x += NUMER statement)" )
              | _ => error "Missing right side ( <expr> = ...?)")
       | L.DO         =>
           (advance();
           let val st = stmt() in
             eat(L.WHILE); eat(L.ONE "(");
             let val ex = cond() in
               eat(L.ONE ")");
               A.Do(st, ex)
             end
           end)
       | L.FOR        => 
           (advance(); eat(L.ONE "(");
            let val loop_var = eatID() in
              eat(L.ONE ",");
              let val for_start = eatNUM() in
                eat(L.ONE ",");
                let val for_end = eatNUM() in
                  eat(L.ONE ")");
                  let val for_stmt = stmt() in
                    case (for_start, for_end) of
                         (A.Num b, A.Num e) => A.For(loop_var, b, e, for_stmt)
                       | _  => error "for(var, begin, end) : begin and end only accepts number literal"
                  end
                end
              end
            end)
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
       | (L.ONE "^")    =>
            (advance();
            let val c_factor = factor() in
              term' (A.App(A.Var("^"), A.Pair(prev_term, c_factor)))
            end)
       | _            => prev_term
  and factor () = 
    case !tok of 
         L.ID str     => (advance(); 
            case !tok of
                 (L.ONE "+") => (advance();
                 case !tok of 
                      (L.ONE "+") => (advance();
                      A.Inc(str))
                    | _ => error "Not implemented")
                | _ => A.Var(str))
       | L.NUM num    => (advance(); A.Num(num))
       | L.ONE "("    => middle advance expr (eat_lazy(L.ONE ")"))
       | L.ONE "-"    => (advance(); A.Neg(expr()))
       | _            => error "Unknown factor"
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
       | _         => error "Unknown conditions operator"

  val print_dec = print o A.inspect_dec
  val print_stmt = print o A.inspect_stmt
  val print_expr = print o A.inspect_expr
end

structure Table = struct
  structure A = Ast
  structure L = Lexer
  exception NoDeclaration of string
  datatype entry = Entry of int * string
 
  val init = nil : entry list

  fun update v nil = [Entry(0,v)]
    | update v (Entry(n,ev)::es) = (Entry(n+1,v))::(Entry(n,ev)::es)

  fun lookup x nil = raise NoDeclaration x
    | lookup x (Entry(n,ev)::es) = 
        if x = ev 
          then Int.toString n
          else lookup x es

  fun stackSize ast = 
    let 
      val stack = ref 0
      fun stack_move n = (stack := ((!stack) + n))
      val maxStack = ref 0
      fun setMaxSize () = 
        if !stack > !maxStack 
        then (maxStack := !stack) 
        else ()
      fun size (A.Def(_,e)) = 
            (size_expr e; 
             stack_move ~1)
        | size (A.If (e, s1, s2)) = 
            (size_expr e; 
             stack_move ~1; 
             size s1;
             case s2 of
                  SOME s =>  size s
                | NONE   => ())
        | size (A.While (e,s)) = 
            (size_expr e;
             stack_move ~1;
             size s)
        | size (A.Do (s,e))    =
            (size s;
             size_expr e)
        | size (A.For (_,_,_,st)) =
            (stack_move 1;
             setMaxSize();
             stack_move ~1;
             size st;
             stack_move 2;
             setMaxSize();
             stack_move ~2)
        | size (A.Iprint e)    = 
            (stack_move 1;
             setMaxSize();
             size_expr(e);
             stack_move ~2)
        | size (A.Scan _) = 
            (stack_move 1;
             setMaxSize();
             stack_move ~1)
        | size (A.Sprint e) = 
            (stack_move 1;
             setMaxSize();
             size_expr e;
             stack_move ~2)
        | size (A.Block (_, ss)) = app size ss
        | size _   = ()
      and size_expr (A.Num _) = (stack_move 1; setMaxSize())
        | size_expr (A.Var _) = (stack_move 1; setMaxSize())
        | size_expr (A.String _) = (stack_move 1; setMaxSize())
        | size_expr (A.App (_, e)) = 
            (size_expr e; stack_move 1;setMaxSize())
        | size_expr (A.Pair (e1, e2)) = 
            (size_expr e1; size_expr e2;stack_move ~2)
        | size_expr (A.Inc s) = 
            (stack_move 2; setMaxSize(); stack_move ~2)
        | size_expr (A.Neg e) = size_expr e
    in
      (size ast; !maxStack)
    end

  fun localSize (A.Block (decs, ss)) = 
      let val n = case decs of
                       A.Dec l => List.foldl (fn (_,c) => c+1) 0 l 
                     | NilDec  => 0 
      in
        List.foldl (fn (s,c) => c + localSize s) n ss 
      end
    | localSize (A.If (_, s1, s2)) = 
        (localSize s1) + (case s2 of SOME s => localSize s | NONE => 0)
    | localSize (A.While (_, s))   = localSize s
    | localSize (A.Do (s,_)) = localSize s
    | localSize (A.For(_,_,_,s)) = 1 + (localSize s)
    | localSize _   = 0

  fun calc_size ast = (localSize ast, stackSize ast)
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
  val is_emit_power_func = ref false

  val label = ref 0
  fun incLabel () = (label := !label + 1; !label)

  fun emit_dec d env = 
    case d of
         A.Dec (sl)   => foldl (fn (s,e) => T.update s e) env sl
       | A.NilDec   => env
  and emit_stmt s env =
    case s of
         A.Def (s,e) => 
           (emit_expr e env;
            out_m ("istore "^(T.lookup s env)))
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
       | A.Do (s,e)     =>
          let val do_start = incLabel() in
            out_label do_start; (* Start *)
            emit_stmt s env;
            let val do_end = emit_expr e env in
              out_goto do_start;
              out_label do_end
            end
          end
       | A.For(var,beg,en,st) =>
           (let val env' = (T.lookup var env ;env) 
                  handle NoDeclaration => emit_dec (A.Dec([var])) env in
              emit_stmt (A.Def(var, A.Num(beg))) env';
              let val for_start_label = incLabel() in
                out_label for_start_label;
                emit_stmt st env';
                out_m ("iinc "^(T.lookup var env')^" 1");
                out_m ("iload "^(T.lookup var env'));
                out_m ("ldc "^Int.toString(en));
                out_m ("if_icmple L"^Int.toString(for_start_label))
              end
            end)
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
              "istore "^(T.lookup s env)
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
                | A.Var "^" => 
                    (is_emit_power_func := true;
                     out_m ("invokestatic Aout/power(II)I");0)
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
      | A.Var s => (out_m ("iload "^(T.lookup s env));0)
      | A.Num s => (out_m ("ldc "^(Int.toString s));0)
      | A.String s => (out_m ("ldc \""^s^"\""); 0)
      | A.Neg e => (emit_expr e env; out_m "ineg";0)
      | A.Inc s => (emit_expr (A.Var s) env; out_m ("iconst_1");out_m("iadd");0)
  fun emit_power_func () = 
    (out (
    ".method static power(II)I\n"^
        "\t.limit locals 2\n"^
        "\t.limit stack 2\n\n"^
        "\tiinc 1 -1\n"^
        "\tiload_0\n"^
    "L_START:\n"^
        "\tiload_0\n"^
        "\timul\n"^
        "\tiinc 1 -1\n"^
        "\tiload_1\n"^
        "\tifle L_END\n"^
        "\tgoto L_START\n"^
    "L_END:\n"^
        "\tireturn\n"^
    ".end method\n"))
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
    "\t.limit locals "^(Int.toString (localSize))^"\n"^
    "\t.limit stack "^(Int.toString stackSize)^"\n");
    emit_stmt ast T.init;
    out(
    "\treturn\n"^
    ".end method\n");
    if !is_emit_power_func then emit_power_func() else ())


  fun reset () = (
    label := 0;
    is_emit_power_func := false)
end
(* }}} *)

fun test () = 
  (Emitter.ostream := TextIO.openOut "tmp.j";
   let 
     val ast = Parser.parse() 
     val (local_size, stack_size) = Table.calc_size ast
   in
     Emitter.emit ast (local_size + 1) (stack_size);
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
            let 
              val ast = Parser.parse() 
              val (local_size, stack_size) = Table.calc_size ast
            in
              Emitter.emit ast (local_size + 1) stack_size;
              TextIO.closeOut (!Emitter.ostream);
              OS.Process.system "jasmin tmp.j"
            end)
in
  exec(); OS.Process.success
end

fun debug () = 
  (Emitter.ostream := TextIO.stdOut;
     Emitter.reset;
   let 
     val ast = Parser.parse() 
     val (local_size, stack_size) = Table.calc_size ast
   in
     print "--- AST:\n";
     Parser.print_stmt ast;
     print "\n--- Java Bytecode:\n";
     Emitter.emit ast (local_size + 1) stack_size
   end)
