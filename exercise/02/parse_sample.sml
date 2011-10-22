
exception Fail

infix 3 &
infix 2 ||

fun f1 & f2 = (fn s => f2(f1 s))
fun f1 || f2 = (fn s => f1 s handle Fail => f2 s)

fun quote (t:string) ([]: string list) = raise Fail
  | quote t (x::rest) = if t = x then rest else raise Fail

val var = quote "x" || quote "y" || quote "z"
val const = quote "0" || quote "1" || quote "2"

fun aexpr s = (var || const || quote "(" & expr & quote ")") s
and expr s = (aexpr & quote "+" & expr || 
            aexpr & quote "*" & expr ||
            aexpr) s
and stmt s = (var & quote ":=" & expr || 
              quote "if" & expr & quote "then" & stmt || 
              quote "while" & expr & quote "do" & stmt ||
              quote "begin" & stmts & quote "end") s
and stmts s = (stmt & quote ";" & stmts || stmt) s
and decl s = (var & quote ":" & quote "integer" || 
              var & quote ":" & quote "boolean") s
and decls s = (decl & quote ";" & decls || decl) s
and prog s = (quote "var" & decls & quote "begin" & stmts & quote "end" & quote ".") s

fun parse s = (if prog s = [] then "YES" else "NO") handle Fail => "NO"
