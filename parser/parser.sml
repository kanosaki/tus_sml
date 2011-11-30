

structure L = Lexer
structure A = Ast

val istream = ref TextIO.stdin

fun getToken () = L.gettoken (!istream)
val tok = ref (L.ONE "")
fun advance () = tok := getToken()

exception Syntax_Error
fun error () = raise Syntax_Error

fun eat t = if (!tok = t) then advance() else error()
fun eatID () = case !tok of
                    (L.ID str) => (advance(); str)
                  | _          => error()

fun eatNUM () = case !tok of 
                     (L.NUM _) => advance()
                   | _         => error()

fun parse () = (advance(); P())
