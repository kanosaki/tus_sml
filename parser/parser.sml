use "lexer.sml";
structure L = Lexer
structure A = Ast

val istream = ref TextIO.stdIn

fun getToken () = L.gettoken (!istream)
val tok = ref (L.ONE "")
fun advance () = tok := getToken()

exception Syntax_error
fun error () = raise Syntax_error

fun eat t =
  if (!tok = t) 
    then advance()
    else error()

fun eatID () =
  case !tok of
       (L.ID str) => (advance(); str)
     | _          => error()
fun eatNUM () = 
  case !tok of 
       (L.NUM _) => advance()
     | _         => error()

fun parse () = (advance(); P()) and
  P ()=
    case !tok of
         L.LET => (eat(L.LET);
          let val str = eatID() 
          in 
            (eat(L.BE);
              let val e1 = E() in (eat (L.IN);
                let val e2 = E()
                in (eat(L.ONE ";"); A.Def (str, e1, e2)) end)
              end)
          end)
       | _     => let val e = E() in (eat (L.ONE ";"); A.Expr e) end and
  E () = let val t = T() in (E' t) end and
  E' e = 
    case !tok of
         (L.ONE "+") => (eat(L.ONE "+");
          let val pre = A.App(A.Var "+", A.Pair (e,T())) in E' (pre) end)
       | (L.ONE "-") => (eat(L.ONE "-");
          let val pre = A.App(A.Var "-", A.Pair(e, T())) in E' (pre) end)
       | _           => e and
  T () = let val f = F() in (T' f) end and
  T' t = 
    case !tok of
         (L.ONE "*") => (eat(L.ONE "*");
          let val pre = A.App(A.Var "*", A.Pair (t,F())) in T' (pre) end)
       | (L.ONE "/") => (eat(L.ONE "/");
          let val pre = A.App(A.Var "/", A.Pair(t, F())) in T' (pre) end)
       | _           => t and
  F () = 
    case !tok of
         (L.ID str) => (eatID(); A.Var str)
       | (L.NUM num) => (eatNUM(); A.Num num)
       | _          => error()


         
