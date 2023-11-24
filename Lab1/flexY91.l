%option noyywrap nodefault yylineno 

%{

 #include <assert.h>
 #include "bisonY91.tab.h"

  static struct { size_t line; size_t column; } current_position = { 1, 1 };

  static size_t line( )   { return current_position.line; }
  static size_t column( ) { return current_position.column; }

  static void add_line( size_t n )   { current_position.line += n; }
  static void add_column( size_t n ) { current_position.column += n; }
  static void drop_column( ) { current_position.column = 1; }
%}

%%
"not"       { return NOT; }
"-"         { return MINUS; }
"+"         { return BIN_PLUS; }
"*"         { return BIN_MUL; }
"/"         { return BIN_DIV; }
"=<"        { return BIN_LESS; }
">="        { return BIN_GREATER; }
"("         { return OP; }
")"         { return CP; }
"["         { return SBO; }
"]"         { return SBC; }
":"         { return COLON;}
","         { return COMMA; }
";"         { return EOEXPR; }
"=="        { return BIN_EQUALS; }
":="        { return ASSIGN; }
"begin"     { return BEGINNING;}
"end"       { return END; }

"method"    { return METHOD; }
"repeat"    { return REPEAT; } 
"until"     { return UNTIL; }
"var"       { return VAR; }
"do"        { return DO;}
"break"     { return BREAK; }
"while"     { return WHILE;}
"of"        { return OF; }

"array"     { return ARRAY; }
"bool"      { return BOOLEAN; }
"byte"      { return BYTE; }
"uint"      { return UINT; }
"int"       { return INT; }
"ulong"     { return ULONG; }
"long"      { return LONG; }
"char"      { return CHAR; }
"string"    { return STRING; }

"if"        { return IF; }
"then"      { return THEN; }
"else"      { return ELSE; }

"false"                       { yylval.value = strdup(yytext); return BOOL; } 
"true"                        { yylval.value = strdup(yytext); return BOOL; } 
\"[^\"\\]*(?:\\.[^\"\\]*)*\"  { yylval.value = strdup(yytext); return STR; } 
'[^']'                        { yylval.value = strdup(yytext); return CHR; }
0[xX][0-9A-Fa-f]+             { yylval.value = strdup(yytext); return HEX; }
0[bB][01]+                    { yylval.value = strdup(yytext); return BITS; }
[0-9]+                        { yylval.value = strdup(yytext); return DEC; }
[a-zA-Z_][a-zA-Z_0-9]*        { yylval.value = strdup(yytext); return IDENT; }

[ \t]       { /* ignore white space */ }
\n          { drop_column( ); add_line( 1 ); yylloc.first_line++; }
.           { assert( "critical error: mystery character\n" ); return ERROR; }

%%
