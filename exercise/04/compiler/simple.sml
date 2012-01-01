
fun $ (x,y) = x y
infixr 0 $

(*{{{ *) (* }}} *)
(* Lexer {{{ *)
structure Lexer = struct
  datatype token = INT 
                 | WHILE | DO | FOR
                 | IF | ELSE 
                 | EQ | GE | LE | NEQ 
                 | LAND | LOR
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
           else if (v = #"=" orelse v = #"!" orelse v = #"<" orelse 
                    v = #">" orelse v = #"|" orelse v = #"&") then
             (read istream;
             let val next = TextIO.lookahead istream in
               case next of
                    NONE      => ONE (String.str v)
                  | SOME nc   =>
                        case (v, nc) of
                             (#"=",#"=") => (read istream;EQ)
                           | (#"!",#"=") => (read istream;NEQ)
                           | (#">",#"=") => (read istream;GE)
                           | (#"<",#"=") => (read istream;LE)
                           | (#"|",#"|") => (read istream;LOR)
                           | (#"&",#"&") => (read istream;LAND)
                           | (_,_)       => ONE (String.str v)
             end)
           else ONE (read istream)
  end
  and proceed_line () = current_line := !current_line + 1
  and gettoken istream =
      let val token = native_token istream in
        case token of 
             ONE " "  => gettoken istream
           | ONE "\t" => gettoken istream
           | ONE "#"  => (drop_comment istream; gettoken istream)
           | ONE "\n" => (proceed_line(); gettoken istream)
           | _        => token
      end
  and drop_comment istream = 
        if (read istream) = "\n" 
          then () 
          else drop_comment istream
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
    | inspect (LAND)     = "LAND"
    | inspect (LOR)      = "LOR"
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

  fun getToken (Src(_, (t,_)::_,_)) = t
    | getToken _ = raise Error

  fun getLine (Src(_, (_,n)::_,_)) = n
    | getLine _ = raise Error

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
       | Inc s    => ( "Inc["^s^"]")


  fun optimze_stmt (Def(s,e)) = Def(s, optimze_expr e)
    | optimze_stmt (If(e, s1, s2)) = If(optimze_expr e, optimze_stmt s1,
        case s2 of
             SOME s => SOME $ optimze_stmt s 
           | NONE => NONE)
    | optimze_stmt (While(e,s)) = While(optimze_expr e, optimze_stmt s)
    | optimze_stmt (For(s,b,e,st)) = For(s,b,e,optimze_stmt st)
    | optimze_stmt (Do(s,e)) = Do(optimze_stmt s, optimze_expr e)
    | optimze_stmt (Iprint e) = Iprint(optimze_expr e)
    | optimze_stmt (Sprint e) = Sprint(optimze_expr e)
    | optimze_stmt (Block(decs,sts)) = Block(decs, map optimze_stmt sts)
    | optimze_stmt ast = ast
  and optimze_expr (App(ope, Pair(Num a, Num b))) = 
        (case ope of
             Var "+" => Num(a + b)
           | Var "-" => Num(a - b)
           | Var "/" => Num(a div b)
           | Var "*" => Num(a * b)
           | _       => App(ope, Pair(Num a, Num b)))
    | optimze_expr (App(v,Pair(Var m, n))) = App(v, Pair(Var m, optimze_expr n))
    | optimze_expr (App(v,Pair(n,Var m))) = App(v, Pair(optimze_expr n, Var m))
    | optimze_expr (App(v,e)) = optimze_expr $ App(v,optimze_expr e)
    | optimze_expr (Neg e) = Neg(optimze_expr e)
    | optimze_expr (Pair(e1,e2)) = Pair(optimze_expr e1, optimze_expr e2)
    | optimze_expr a = a 
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
  structure S = Source
  
  exception SyntaxError of string

  val source = ref $ S.init TextIO.stdIn

  fun tok () = S.getToken (!source)
  fun line_number () = S.getLine (!source)
  fun advance () = 
        case S.next (!source) of
             ((tk, line_no), src) => 
                (source := src)
  fun back () = let val s = S.back (!source) in source := s; s end

  fun init is = (
    source := S.init (is))

  fun error msg  =
  let val msg = ("SYNTAX ERROR!!\n"^msg^"\nLine:"^(Int.toString (line_number()))^
              " LAST TOKEN:"^(L.inspect (tok())))
  in (raise SyntaxError msg) 
  end

  fun eat t = 
    if (tok() = t) 
      then advance() 
      else error ("Missing "^(L.inspect t))
  fun eat_lazy f (_:unit) = eat f
  fun eatID () = 
    case tok() of
         (L.ID s)     => (advance(); s)
       | _            => error "ID required."
  fun eatNUM () =
    case tok() of
         (L.NUM n)    => (advance(); (A.Num n))
       | _            => error "Number requried."
  fun eatSTR () =
    case tok() of
         (L.STRING s)    => (advance(); (A.String s))
       | _            => error "String required."

  fun parse () = (advance(); stmt())
  and dec () = 
    case tok() of
         L.INT    => A.Dec(middle advance ids (eat_lazy(L.ONE ";")))
       | _        => A.NilDec
  and ids () = (eatID())::(ids'())
  and ids' () =
    case tok() of
         (L.ONE ",")    => (advance(); (eatID()) :: (ids'()))
       | _              => nil
  and stmts () = 
    case tok() of
         L.ONE "}"    => nil
       | _            => (stmt()) :: (stmts())
  and stmt ()=
    case tok() of
         L.ID str     => 
           (advance(); 
           case tok() of
                (L.ONE "=") =>
                   A.Def(str, middle (eat_lazy(L.ONE "=")) expr (eat_lazy(L.ONE ";")))
              | (L.ONE "+") =>
                  (advance();
                  case tok() of
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
    case tok() of
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
    case tok() of
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
    case tok() of
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
    case tok() of 
         L.ID str     => (advance(); 
            case tok() of
                 (L.ONE "+") => (advance();
                 case tok() of 
                      (L.ONE "+") => (advance();
                      A.Inc(str))
                    | _ => (back(); A.Var(str)))
                | _ => A.Var(str))
       | L.NUM num    => (advance(); A.Num(num))
       | L.ONE "("    => middle advance expr (eat_lazy(L.ONE ")"))
       | L.ONE "-"    => (advance(); A.Neg(expr()))
       | _            => error "Unknown factor"
  and cond () = 
    case eval_three expr condop expr of
         (e1,cop,e2)  => A.App(cop, A.Pair(e1,e2))
  and condop () = 
    case tok() of
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
          then n
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

(* Bytecode {{{ *)
structure Bytecode = struct
  datatype java_type = I | V | Class of string | Array of java_type | String
  type label = string
  datatype inst = IntConst of int | StrConst of string
                | Load of int
                | Store of int
                | GetStatic of string * java_type
                | InvokeVirtual of string * java_type list * java_type
                | InvokeStatic of string * java_type list * java_type
                | InvokeNonVirtual of string * java_type list * java_type
                | GoTo of label
                | Increase of int * int
                | Add | Sub | Mul | Div | Rem | Neg
                | CmpLe of label | CmpLt of label
                | CmpGe of label | CmpGt of label 
                | CmpEq of label | CmpNe of label
                | IfLe of label | IfLt of label 
                | IfGe of label | IfGt of label 
                | IfEq of label | IfNe of label
                | Return | IReturn
                | Label of label
  type code = inst list

  fun conv_type I = "I"
    | conv_type V = "V"
    | conv_type String = conv_type $ Class("java/lang/String")
    | conv_type (Class s) = "L" ^ s ^ ";"
    | conv_type (Array t) =  "[" ^ conv_type t

  fun conv_args types = foldl (op^) "" $ map conv_type types 

  fun conv_inst (IntConst i) =
        if 0 <= i andalso i <= 5 
          then "iconst_" ^ (Int.toString i)
          else "ldc " ^ (Int.toString i)
    | conv_inst (StrConst s) = "ldc \"" ^ s ^ "\""
    | conv_inst (Load i) =
        if 0 <= i andalso i <= 3 
          then "iload_" ^ (Int.toString i)
          else "iload " ^ (Int.toString i)
    | conv_inst (Store i) = 
        if 0 <= i andalso i <= 3 
          then "istore_" ^ (Int.toString i)
          else "istore " ^ (Int.toString i)
    | conv_inst (GetStatic (path, t)) = "getstatic "^ path ^ " " ^ (conv_type t)
    | conv_inst (InvokeVirtual(path, args, t)) = 
        "invokevirtual " ^ path ^ "(" ^ (conv_args args) ^ ")" ^ (conv_type t)
    | conv_inst (InvokeNonVirtual(path, args, t)) = 
        "invokenonvirtual " ^ path ^ "(" ^ (conv_args args) ^ ")" ^ (conv_type t)
    | conv_inst (InvokeStatic(path, args, t)) = 
        "invokestatic " ^ path ^ "(" ^ (conv_args args) ^ ")" ^ (conv_type t)
    | conv_inst (GoTo s) = "goto " ^ s
    | conv_inst (Increase(loc, v)) = 
        "iinc "^(Int.toString loc)^" "^(Int.toString v)
    | conv_inst Add = "iadd"
    | conv_inst Sub = "isub"
    | conv_inst Mul = "imul"
    | conv_inst Div = "idiv"
    | conv_inst Rem = "irem"
    | conv_inst Neg = "ineg"
    | conv_inst (CmpLe l) = "if_icmple " ^ l
    | conv_inst (CmpLt l) = "if_icmplt " ^ l
    | conv_inst (CmpGe l) = "if_icmpge " ^ l
    | conv_inst (CmpGt l) = "if_icmpgt " ^ l
    | conv_inst (CmpEq l) = "if_icmpeq " ^ l
    | conv_inst (CmpNe l) = "if_icmpne " ^ l
    | conv_inst (IfLe l) = "ifle " ^ l
    | conv_inst (IfLt l) = "iflt " ^ l
    | conv_inst (IfGe l) = "ifge " ^ l
    | conv_inst (IfGt l) = "ifgt " ^ l
    | conv_inst (IfEq l) = "ifeq " ^ l
    | conv_inst (IfNe l) = "ifne " ^ l
    | conv_inst (Return) = "return"
    | conv_inst (IReturn) = "ireturn"
    | conv_inst (Label l) = l
  
  (* NOTE: Instructions are reversed. *)
  fun optimize ((Store i)::(Load j)::xs) =
        if i = j
          then optimize xs
          else (Store i)::(Load j)::(optimize xs)
    | optimize (x::xs) = x :: (optimize xs)
    | optimize nil = nil
end
(* }}} *)

(* Output{{{ *) 
structure Output = struct
  structure B = Bytecode
  exception NotSupported
  datatype proceedure = Function of 
                               string (* Name *) 
                               * B.java_type list (* Argument Types *)
                               * B.java_type (* Return type *)
                               * int  (* Local Size *)
                               * int  (* Stack Size *)
                               * B.inst list (* Instructions *)
                      | Raw of string * string
  datatype pool = Pool of TextIO.outstream * proceedure list 
  fun init_pool ostream = Pool(ostream, nil)
  fun push inst (Function(name, args, ret ,szLoc, szSt, insts)) =
    Function(name, args, ret, szLoc, szSt, inst::insts)
    | push _ (Raw(_,_)) = raise NotSupported
  fun store proc (Pool(out, procs)) = Pool(out, proc::procs)

  fun indent_inst (B.Label l) = l ^ ":\n"
    | indent_inst els       = "\t" ^ B.conv_inst els ^ "\n"

  val header = 
    (".class synchronized Aout\n"^
    ".super java/lang/Object\n"^
    ".method <init>()V\n"^
      "\t.limit locals 1\n"^
      "\t.limit stack 1\n"^
      "\taload_0\n"^
      "\tinvokenonvirtual java/lang/Object.<init>()V\n"^
      "\treturn\n"^
    ".end method\n\n")
  fun conv_proc (Function(name, args, ret ,szLoc, szSt, insts)) = 
    ".method public static "^name^"("^(B.conv_args args)^")"^(B.conv_type ret)^"\n"^
    "\t.limit locals "^(Int.toString (szLoc))^"\n"^
    "\t.limit stack "^(Int.toString szSt)^"\n"^
    (foldl (op^) "" (map indent_inst insts)^
    ".end method\n\n")
    | conv_proc (Raw (_, s)) = s

  fun flush (Pool(out, procs)) = 
  let fun output s = TextIO.output(out, s) in
     output header;
     app output (map conv_proc procs)
  end
end
(* }}} *)

(* Emitter {{{ *) 
structure Emitter = struct 
  structure A = Ast
  structure T = Table
  structure B = Bytecode
  structure O = Output
  
  exception InternalError

  val out = ref (nil:B.inst list)
  fun push inst = out := (inst :: (!out)) 

  val label = ref 0
  fun incLabel () = (label := !label + 1; !label)
  fun conv_label i = "L"^(Int.toString i)

  fun init () = (label := 0; out := nil)

  fun emit_dec d env = 
    case d of
         A.Dec (sl)   => foldl (fn (s,e) => T.update s e) env sl
       | A.NilDec   => env
  and emit_stmt s env =
  let 
    fun lookup_var s = T.lookup s env
    fun push_label i = push (B.Label(conv_label i))
    fun push_goto i = push (B.GoTo(conv_label i))
  in
    case s of
         A.Def (s,e) => 
           (emit_expr e env;
            push (B.Store(lookup_var s)))
       | A.If (c,s, opt) => 
           let 
             val i = emit_expr c env
             val j = incLabel()  
           in
             emit_stmt s env;
             case opt of 
                  SOME s2 => 
                    (push_goto j;
                     push_label i;
                     emit_stmt s2 env;
                     push_label j)
                | NONE    => (push_label i)
           end
       | A.While (c,s)  => 
           let val i = incLabel() in
             push_label i;
             let val j = emit_expr c env in
               emit_stmt s env;
               push_goto i;
               push_label j
             end
           end
       | A.Do (s,e)     =>
          let val do_start = incLabel() in
            push_label do_start; (* Start *)
            emit_stmt s env;
            let val do_end = emit_expr e env in
              push_goto do_start;
              push_label do_end
            end
          end
       | A.For(var,begin_num,end_num,st) =>
           (let val env' = (lookup_var var;env) 
                  handle NoDeclaration => emit_dec (A.Dec([var])) env in
              emit_stmt (A.Def(var, A.Num(begin_num))) env';
              let 
                val for_start_label = incLabel() 
                val counter_var = T.lookup var env'
              in
                push_label for_start_label;
                emit_stmt st env';
                push $ B.Increase(counter_var, 1);
                push $ B.Load(counter_var);
                push $ B.IntConst(end_num);
                push $ B.CmpLe(conv_label for_start_label)
              end
            end)
       | A.Sprint s     => 
           (push $ B.GetStatic("java/lang/System/out", B.Class("java/io/PrintStream"));
            emit_expr s env;
            push $ 
                 B.InvokeVirtual("java/io/PrintStream/print", 
                [B.Class("java/lang/String")], B.V))
       | A.Iprint e     =>
           (push $ B.GetStatic("java/lang/System/out", B.Class("java/io/PrintStream"));
            emit_expr e env;
            push (B.InvokeVirtual("java/io/PrintStream/print", [B.I], B.V)))
       | A.Scan s       => 
           (push $ B.InvokeStatic("Scan/scan", nil, B.I);
            push $ B.Store(lookup_var s))
       | A.NilStmt      => ()
       | A.Block (d,l)  => 
           let 
             val env' = emit_dec d env
           in 
             app (fn st => emit_stmt st env') l
           end
  end
  and emit_expr ast env =
    case ast of
         A.App (e1, e2) =>
            (emit_expr e2 env;
             case e1 of
                  A.Var "+" => (push B.Add;0)
                | A.Var "-" => (push B.Sub;0)
                | A.Var "*" => (push B.Mul;0)
                | A.Var "/" => (push B.Div;0)
                | A.Var "!" => (push B.Neg;0)
                | A.Var "%" => (push B.Rem;0)
                | A.Var "^" => 
                    (push (B.InvokeStatic("Aout/power", [B.I, B.I], B.I));0)
                | A.Var "EQ" =>
                    let val n = incLabel() in
                      push (B.CmpNe(conv_label n));n
                    end
                | A.Var "NEQ" => 
                    let val n = incLabel() in
                      push (B.CmpEq(conv_label n));n
                    end
                | A.Var "GT" =>  
                    let val n = incLabel() in
                      push (B.CmpLe(conv_label n));n
                    end
                | A.Var "LT" => 
                    let val n = incLabel() in
                      push (B.CmpGe(conv_label n));n
                    end
                | A.Var "GE" => 
                    let val n = incLabel() in
                      push (B.CmpLt(conv_label n));n
                    end
                | A.Var "LE" => 
                    let val n = incLabel() in
                      push (B.CmpGt(conv_label n));n
                    end
                | _          => raise InternalError)
      | A.Pair (e1, e2) => (emit_expr e1 env; emit_expr e2 env)
      | A.Var s => (push (B.Load(T.lookup s env));0)
      | A.Num i => (push (B.IntConst i);0)
      | A.String s => (push (B.StrConst s); 0)
      | A.Neg e => (emit_expr e env; push (B.Neg);0)
      | A.Inc s => 
          let val var_num = T.lookup s env in
            emit_expr (A.Var s) env; 
            push (B.IntConst 1);
            push B.Add;
            push (B.Store var_num);
            push (B.Load var_num);
            0
          end
  val power_func = O.Raw("power",
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
    ".end method\n")
  
  fun generate ast = (emit_stmt ast T.init; B.Return :: (!out)) 

  fun emit_optimize ast outstream = 
  let 
    val main_code = (init();B.optimize $ generate ast)
    val (szLocal, szStack) = T.calc_size ast
    val main = 
      O.Function("main", [B.Array(B.String)], B.V, szLocal + 1, szStack, main_code)
    val is_emit_pow = 
      List.exists 
        (fn x => case x of
                      (B.InvokeStatic ("Aout/power",_,_)) => true 
                    | _ => false) main_code
    val pool = 
      O.Pool(outstream, (if is_emit_pow then [main, power_func] else [main]))
  in
    O.flush pool
  end

  fun emit ast outstream = 
  let 
    val main_code = (init();generate ast)
    val (szLocal, szStack) = T.calc_size ast
    val main = 
      O.Function("main", [B.Array(B.String)], B.V, szLocal + 1, szStack, main_code)
    val is_emit_pow = 
      List.exists 
        (fn x => case x of
                      (B.InvokeStatic ("Aout/power",_,_)) => true 
                    | _ => false) main_code
    val pool = 
      O.Pool(outstream, (if is_emit_pow then [main, power_func] else [main]))
  in
    O.flush pool
  end
end
(* }}} *)
(*
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
*)
fun err str = TextIO.output(TextIO.stdErr, str)
fun main (name, args) = 
let 
  fun exec () = 
    case args of
         nil => (err (name^": missing file name\n");
                 OS.Process.exit OS.Process.failure)
       | (hd::tl)   => 
           (Parser.init $ TextIO.openIn hd;
            let 
              val ast = Parser.parse() 
              val ostream = TextIO.openOut "tmp.j"
            in
              Emitter.emit ast ostream;
              TextIO.closeOut ostream;
              OS.Process.system "jasmin tmp.j"
            end 
            handle 
              (Parser.SyntaxError msg) => (print (msg^"\n"); OS.Process.failure)
            | (Table.NoDeclaration s) => (print ("Undeclared variable!!:"^s^"\n"); OS.Process.failure)
            | _ => (print "Unknown Error.\n"; OS.Process.failure))
in
  exec(); OS.Process.success
end

fun debug () = 
  (let 
     val ast = Parser.parse() 
     val optimized_ast = Ast.optimze_stmt ast
   in
     print "--- AST:\n";
     Parser.print_stmt ast; 
     print "\n\n--- Optimized AST\n";
     Parser.print_stmt optimized_ast;
     print "\n\n--- Java Bytecode:\n";
     Emitter.emit ast TextIO.stdOut;
     print "\n\n--- Optimized Java Bytecode:\n";
     Emitter.emit_optimize optimized_ast TextIO.stdOut
   end 
   handle 
     (Parser.SyntaxError msg) => (print (msg^"\n"))
   | (Table.NoDeclaration s) => (print ("SYNTAX ERROR: Undeclared variable!!"^s^"\n"))
   | _ => (print "Unknown Error.\n"))

