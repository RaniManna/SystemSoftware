%locations

%code requires {
  #include <stdbool.h>
  #include "tree/Ast.h"
}
%{

  #include <stdio.h>
  #include <stdlib.h>
  #include <getopt.h>

  extern FILE* yyin;
  extern FILE* yyout;

  extern int yylex(void);
  void yyerror( char const* s );

  int is_verbose = 0;
  static struct ast_node* root = NULL; // pointer to the root of AST
%}

%union {
  struct ast_node* node;
  char*        value;
 // char*        name;
  //bool        bl;
  //char        cr;
  enum operation_type op;
}

//%token IDENT BOOL CHR STR BITS DEC HEX 

/* declare tokens */                                                              


                                           
%token <value>  IDENT
%token <value> BOOL CHR HEX BITS DEC STR 
%left       <op>  NOT
%left       <op> MINUS

%left       <op>  BIN_PLUS  
%left       <op>  BIN_MUL BIN_DIV BIN_LESS BIN_GREATER BIN_EQUALS        
%right            ASSIGN     
//%left             UMINUS

%token    OP CP SBO SBC      //%token ?   
%token    WHILE
%token         REPEAT UNTIL DO BREAK                                                 
%token         EOEXPR COMMA COLON                                                 
%token         VAR BEGINNING END METHOD
%token         IF
%token         BOOLEAN BYTE INT UINT LONG ULONG CHAR STRING ARRAY OF
%token    THEN
%token    ELSE

//%precedence         BOOL, STR, CHR, BITS, DEC, 

//%precedence         REPEAT UNTIL WHILE DO BREAK                                                 
//%precedence         EOEXPR COMMA COLON
//%precedence BOOLEAN BYTE INT UINT LONG ULONG CHAR STRING ARRAY OF
//%precedence          IF THEN ELSE
//%precedence VAR BEGINNING END METHOD


%token              ERROR // used in lexer to return error                        

//%token <value>  BOOL
//%token <value>  CHR
//%token <value>   STR
//%token <value>  BITS 
//%token <value>   DEC 
//%token <value>   HEX
//%token <value>  IDENT

//%type <op> unop binop                                                             
%type <node> expr
%type <node> typeRef
%type <node> statement statement_list 
%type <node> program                                                              
%type <node> literal
//%type <node> liretal 
//%token literal

%type <node> funcDef list
%type <node> array array_elements
%type <node> argDef argDef_list 
//%type <node> insert_stat_list
//%type <node> insert_literal_list insert_argdef_list 
%type <node>funcSignature body
%type <node> expr_list statement_block
//%type <node> liretal BOOL STR CHR BITS DEC 

%start program                                                                    


%%
program: funcDef { $$ = root = make_program( $1 ); }
       ;



typeRef: IDENT     { $$ = make_typeref_ident(make_ident($1, make_typeref(TR_NONE))); }
       | BOOLEAN   { $$ = make_typeref(TR_BOOL); }
       | BYTE      { $$ = make_typeref(TR_BYTE); }
       | INT       { $$ = make_typeref(TR_INT); }
       | UINT      { $$ = make_typeref(TR_UINT); }
       | LONG      { $$ = make_typeref(TR_LONG); }
       | ULONG     { $$ = make_typeref(TR_ULONG); }
       | CHAR      { $$ = make_typeref(TR_CHAR); }
       | STRING    { $$ = make_typeref(TR_STR); }
       | array     { $$ = make_typeref_array($1); }
       ;

array: ARRAY SBO array_elements SBC OF typeRef  { $$ = set_type_array($3, $6); } //todo
  ;

array_elements: IDENT COMMA array_elements      { $$ = insert_array (&$3, make_ident($1, make_typeref(TR_NONE))); }
              | literal COMMA array_elements    { $$ = insert_array (&$3, $1); }
              | literal                         { $$ = make_array($1, NULL); }
              | IDENT                           { $$ = make_array(make_ident($1,  make_typeref(TR_NONE)), NULL); }   
              ;

funcDef: METHOD funcSignature  body  { $$ = make_func ($2, $3); } 
       | METHOD funcSignature EOEXPR { $$ = make_func ($2, NULL); }
       ;

funcSignature: IDENT OP argDef_list CP COLON typeRef {  $$ = make_func_sign (make_ident ($1, $6) , $3); }
             | IDENT OP argDef_list CP  { $$ = make_func_sign (make_ident ($1, make_typeref(TR_NONE)), $3); }
          // {  $$ = make_ident($1); }
               ;

argDef: IDENT COLON typeRef { $$ = make_ident ($1, $3); }
      | IDENT               { $$ = make_ident ($1, make_typeref(TR_NONE)); }
      ;

argDef_list: argDef COMMA argDef_list { $$ = insert_argdef_list(&$3, $1); }
           | argDef                   { $$ = make_argdef_list($1, NULL);}
           ;

body: VAR argDef_list EOEXPR statement_block  { $$ = make_body($2, $4); }
    | statement_block                         { $$ = make_body(NULL, $1); }
    ;

statement: IF expr THEN statement                { $$ = make_statement(make_branch($2, $4, NULL )); }
         | IF expr THEN statement ELSE statement  { $$ = make_statement(make_branch($2, $4, $6 )); }
         | statement_block                        { $$ = make_statement($1);}
         | WHILE expr DO statement                { $$ = make_statement(make_while($2, $4)); }
         | REPEAT statement WHILE expr EOEXPR     { $$ = make_statement(make_repeat($2, $4)); }
         | REPEAT statement UNTIL expr EOEXPR     { $$ = make_statement(make_repeat($2, $4)); }
         | BREAK EOEXPR                           { $$ = make_statement(make_break());}
         | expr EOEXPR                            { $$ = make_statement($1); }
         ;

statement_block: BEGINNING statement_list END { $$ = $2; }
               | BEGINNING END                {$$ = NULL;}
               ;

statement_list: statement statement_list {insert_stat_list(&$2, $1); $$ = $2;}
              | statement                {$$ = make_stat_list($1, NULL); }
              ;

literal: list  {$$=  $1; }
       | BOOL  {$$=  make_literal($1, make_typeref_lt(LT_BOOL)); } // todo fix literal
       | STR   {$$=  make_literal($1, make_typeref_lt(LT_STR)); }
       | CHR   {$$=  make_literal($1, make_typeref_lt(LT_CHAR)); }
       | BITS  {$$=  make_literal($1, make_typeref_lt(LT_BITS)); }
       | HEX   {$$=  make_literal($1, make_typeref_lt(LT_HEX)); }
       | DEC   {$$=  make_literal($1, make_typeref_lt(LT_DEC)); }
       ;

list:  OP literal COMMA list CP  { $$ = insert_literal_list(&$4, $2); }
    |  OP literal CP             { $$ = make_literal_list($2, NULL); }
    ;

//expr: expr binop expr          { $$ = make_binexpr($2, $1, $3);}
//    | unop expr                { $$ = make_unexpr( $1, $2 ); } 
//    | expr OP expr_list CP     { $$ = make_expr_call($1, $3); }
//    | expr SBO expr_list SBC   { $$ = make_expr_indexer($1, $3); }
//    | literal                  { $$ = make_expr($1); }
//    | IDENT                    { $$ = make_expr(make_ident ($1, make_typeref(TR_NONE))); }
//    | OP expr CP               { $$ = $2; }
//    ;

expr: expr MINUS  expr               { $$ = make_binexpr( OT_MINUS     , $1,$3);}
    | expr BIN_PLUS    expr          { $$ = make_binexpr( OT_BIN_PLUS  , $1,$3);}
    | expr BIN_MUL     expr          { $$ = make_binexpr( OT_BIN_MUL   , $1,$3);}
    | expr BIN_DIV     expr          { $$ = make_binexpr( OT_BIN_DIV   , $1,$3);}
    | expr BIN_LESS    expr          { $$ = make_binexpr( OT_BIN_LESS  , $1,$3);}
    | expr BIN_GREATER expr          { $$ = make_binexpr( OT_BIN_GREATER,$1, $3);}
    | expr BIN_EQUALS  expr          { $$ = make_binexpr( OT_BIN_EQUALS, $1,$3);}
    | expr ASSIGN      expr          { $$ = make_binexpr( OT_ASSIGN    , $1,$3);}
    | NOT expr                 { $$ = make_unexpr( OT_UN_NOT, $2 ); } 
    | expr OP expr_list CP     { $$ = make_expr_call($1, $3); }
    | expr SBO expr_list SBC   { $$ = make_expr_indexer($1, $3); }
    | literal                  { $$ = make_expr($1); }
    | IDENT                    { $$ = make_expr(make_ident ($1, make_typeref(TR_NONE))); }
    | OP expr CP              { $$ = $2; }
    ;

expr_list: expr COMMA expr_list   {$$ = insert_expr_list(&$3, $1); }
         | expr                   {$$ = make_expr_list($1, NULL); }
         ;

//unop:  NOT       { $$ = OT_UN_NOT; } //MINUS  {$$ = OT_MINUS;  }
// ;

//binop: MINUS    { $$ = OT_MINUS;       }
// | BIN_PLUS     { $$ = OT_BIN_PLUS;    }
// | BIN_MUL      { $$ = OT_BIN_MUL;     }
// | BIN_DIV      { $$ = OT_BIN_DIV;     }
// | BIN_LESS     { $$ = OT_BIN_LESS;    }
// | BIN_GREATER  { $$ = OT_BIN_GREATER; }
// | BIN_EQUALS   { $$ = OT_BIN_EQUALS;  }
// | ASSIGN       { $$ = OT_ASSIGN;      }
// ;

%%

void
yyerror(const char *s)
{
 // fprintf(stderr,"error: %s on line %d\n", s, yylineno);
}

int main( int argc, char** argv ) {                                                      
  int optidx = 0;                                                                        
  int is_input = 0;                                                                      
  struct option options[] = {                                                            
    { "file",    required_argument, NULL, 'f' },                                         
    { "help",    no_argument,       NULL, 'h' },                                         
    { 0, 0, 0, 0 }                                                                       
  };                                                                                     
                                                                                         
  char brief_option;                                                                     
  while ( -1 != (brief_option = getopt_long( argc, argv, "vhf:o:", options, &optidx )) ) 
    switch ( brief_option ) {                                                            
      case 'h':                                                                          
        fprintf( stdout,                                                                 
          "SYNOPSYS"                                                                     
          "\n\tspc [-v] [-f <input file>]"                                               
          "\nDESCRIPTION"                                                                
          "\n\t-h, --help"                                                               
          "\n\t\tshows this help message and exits"                                      
          "\n\t-f, --file"                                                               
          "\n\t\tspecifies the input file path, default: stdin"                          
        );                                                                               
        return 0;                                                                        
      case 'f':                                                                          
        yyin = fopen( optarg, "r" ); is_input = 1; break;                                
      default: { 
      printf("can't open file");
      exit(-1);     
        }
    }                                                                                    
                                                                                         
  bool parse_result = yyparse( );                                                        
  if ( is_input )                                                                        
    fclose( yyin );                                                                      
                                                                                         
  // traverse AST for generating TAC                                                     
  if ( root == NULL ) printf("node is null\n");
                                                               
   //is_verbose && fprintf( stdout, "can't find root\n" );                                
  else {          
   print_ast( root );                                                                   
  //  print_tac( root );                                                                   
  }                                                                                      
  //                                                                                       
  //// free the ast                                                                        
  //free_ast( root );                                                                      
  return parse_result;                                                                   
}                                                                                        

