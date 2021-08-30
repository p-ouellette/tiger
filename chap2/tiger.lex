type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val err = ErrorMsg.error
val commentLvl = ref 0
val strStart = ref 0
val strParts = ref ([]:string list)

fun newline pos = (lineNum := !lineNum+1; linePos := pos :: !linePos)

fun makeInt (s, pos) = Tokens.INT(valOf(Int.fromString s),pos,pos+size s)

fun makeString endPos =
  let val s = valOf(String.fromString(concat(rev(!strParts))))
  in
    strParts := [];
    Tokens.STRING(s,!strStart,endPos)
  end

fun eof() =
  let val pos = hd(!linePos)
  in
    if !commentLvl <> 0 then err pos "unclosed comment" else ();
    Tokens.EOF(pos,pos)
  end

%%
%s COMMENT STRING FCHARS;
id=[A-Za-z][A-Za-z0-9_]*;
int=[0-9]+;
str=([^\000-\031\127\"\\]|\\[nt\"\\]|\\\^[\064-\095]|\\[0-9]{3})*;
fchars=[ \t\012]+;
%%
<INITIAL,COMMENT,FCHARS>\n => (newline yypos; continue());
<INITIAL>while    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>for      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>to       => (Tokens.TO(yypos,yypos+2));
<INITIAL>break    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>let      => (Tokens.LET(yypos,yypos+3));
<INITIAL>in       => (Tokens.IN(yypos,yypos+2));
<INITIAL>end      => (Tokens.END(yypos,yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>var      => (Tokens.VAR(yypos,yypos+3));
<INITIAL>type     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array    => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if       => (Tokens.IF(yypos,yypos+2));
<INITIAL>then     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>else     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>do       => (Tokens.DO(yypos,yypos+2));
<INITIAL>of       => (Tokens.OF(yypos,yypos+2));
<INITIAL>nil      => (Tokens.NIL(yypos,yypos+3));
<INITIAL>","      => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"      => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"      => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("      => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"      => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["      => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"      => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"      => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"      => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."      => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"      => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"      => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"      => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"      => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="      => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"     => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"      => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<="     => (Tokens.LE(yypos,yypos+2));
<INITIAL>">"      => (Tokens.GT(yypos,yypos+1));
<INITIAL>">="     => (Tokens.GE(yypos,yypos+2));
<INITIAL>"&"      => (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"      => (Tokens.OR(yypos,yypos+1));
<INITIAL>":="     => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>{id}     => (Tokens.ID(yytext,yypos,yypos+size yytext));
<INITIAL>{int}    => (makeInt(yytext,yypos));

<INITIAL>\"       => (YYBEGIN STRING; strStart := yypos; continue());
<STRING>{str}     => (strParts := yytext :: !strParts; continue());
<STRING>\\[\033-\126] => (err yypos "illegal string escape"; continue());
<STRING>\\        => (YYBEGIN FCHARS; continue());
<FCHARS>{fchars}  => (continue());
<FCHARS>\\        => (YYBEGIN STRING; continue());
<FCHARS>.         => (err yypos "unclosed string"; continue());
<STRING>\"        => (YYBEGIN INITIAL; makeString(yypos+1));
<STRING>\n        => (newline yypos; err yypos "unclosed string"; continue());
<STRING>.         => (err yypos "illegal non-printing character in string";
                      continue());

"/*"              => (YYBEGIN COMMENT; commentLvl := !commentLvl+1; continue());
<COMMENT>"*/"     => (commentLvl := !commentLvl-1;
                      if !commentLvl = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT>.        => (continue());

<INITIAL>{fchars} => (continue());
.                 => (err yypos ("illegal character " ^ yytext); continue());
