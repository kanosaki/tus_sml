structure Lexer = struct
  datatype token = LET | BE  | IN 
                 | ID of string
                 | NUM of int
                 | ONE of string
                 | EOF

  fun read istream = 
        case TextIO.input1 istream of
             SOME c => String.str c
           | NONE   => ""
  and integer istream i =
    let
      val c = TextIO.lookahead istream 
    in 
      case c of 
           NONE => 0
         | SOME v => 
             (if (Char.isDigit v) 
              then integer istream 
                    (10 * i + ord(String.sub(read istream, 0)) - ord(#"0"))
              else i)
    end
        
  and identifier istream id = 
    let 
      val c = TextIO.lookahead istream 
    in 
      case c of 
           NONE   => ""
         | SOME v =>
             if (Char.isLower v) orelse (Char.isUpper v)
                orelse (Char.isDigit v) orelse v = #"_" 
             then identifier istream (id ^ (read istream))
             else id
    end
  and native_token istream = 
      let 
        val c = TextIO.lookahead istream 
      in
        case c of 
             NONE => EOF
           | SOME v =>
               if (Char.isLower v) orelse (Char.isUpper v)
               then
                 let 
                   val id = identifier istream "" 
                 in 
                   case id of
                        "let" => LET
                      | "be" => BE
                      | "in" => IN
                      | _ => ID (id)
                 end
               else if (Char.isDigit v) then NUM (integer istream 0)
               else ONE (read istream)
     end
  and gettoken istream = 
    let 
      val token = native_token istream 
    in
      case token of 
           ONE " " => gettoken istream
         | ONE "\t" => gettoken istream
         | ONE "\n" => gettoken istream
         | _       => token
    end
end
structure Ast = struct
  datatype definition = Def of (string * expr * expr) 
                      | Expr of expr
  and expr = Num of int | Var of string | App of expr * expr | Pair of expr * expr
end


