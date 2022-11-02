{
  open Parser
}

rule tokenise = parse
| [' ' '\t' '\n'] { tokenise lexbuf }
| ['a'-'z']+ as i { IDENT i }
| '(' { LPAR }
| ')' { RPAR }
| ',' { COMMA }
| '_' { WILDCARD }
| eof { EOF }
