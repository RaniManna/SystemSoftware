#define MAXIMUM_IDENTIFIER_LENGTH 256
#include <stdbool.h>

enum operation_type {
  OT_BIN_PLUS,
  OT_MINUS,

  OT_BIN_MUL,
  OT_BIN_DIV,

  OT_BIN_LESS,
  OT_BIN_EQUALS,
  OT_BIN_GREATER,

  OT_ASSIGN,

  OT_UN_NOT
};

enum ast_node_type {
  T_IDENT,
  T_LITERAL,
  T_EXPR,
  T_UN_EXPR,
  T_BIN_EXPR,
  T_CALL_EXPR,
  T_REPEAT,
  T_BRANCH,
  T_STMTS_LIST,
  T_PROGRAM,
  T_WHILE,
  T_EXPR_LIST,
  T_ARGDEF_LIST,
  T_ARRAY,
  T_LIT_LIST,
  T_INDEXER,
  T_FUNC,
  T_FUNC_SIGN,
  T_BODY,
  T_TYPE_REF,
  T_BREAK,
  T_TYPE_REF_LT,
  T_STMT
};

enum type_ref {
  TR_IDENT,
  TR_BOOL,
  TR_BYTE,
  TR_INT,
  TR_UINT,
  TR_LONG,
  TR_ULONG,
  TR_CHAR,
  TR_STR,
  TR_ARR,
  TR_NONE
};

enum literal_type { LT_BOOL, LT_STR, LT_CHAR, LT_BITS, LT_DEC, LT_HEX };

static const char *literal_names[] = {
    [LT_BOOL] = "bool", [LT_STR] = "str", [LT_CHAR] = "char",
    [LT_BITS] = "bits", [LT_DEC] = "dec", [LT_HEX] = "hex"};

static const char *typeref_names[] = {
    [TR_IDENT] = "ident", [TR_BOOL] = "bool", [TR_BYTE] = "byte",
    [TR_INT] = "int",     [TR_UINT] = "uint", [TR_LONG] = "long",
    [TR_ULONG] = "ulong", [TR_CHAR] = "char", [TR_STR] = "str",
    [TR_ARR] = "array",   [TR_NONE] = "none"};

static const char *ant_names[] = {[T_IDENT] = "identifier",
                                  [T_LITERAL] = "literal",
                                  [T_EXPR] = "expression",
                                  [T_EXPR_LIST] = "expression_list",
                                  [T_UN_EXPR] = "unary-expression",
                                  [T_REPEAT] = "repeat-until",
                                  [T_STMT] = "statement",
                                  [T_STMTS_LIST] = "statements-list",
                                  [T_PROGRAM] = "program",
                                  [T_CALL_EXPR] = "call-expression",
                                  [T_INDEXER] = "indexer-expression",
                                  [T_BRANCH] = "branch",
                                  [T_WHILE] = "while",
                                  [T_ARGDEF_LIST] = "arg-def-list",
                                  [T_ARRAY] = "array",
                                  [T_LIT_LIST] = "literal-list",
                                  [T_FUNC] = "func",
                                  [T_FUNC_SIGN] = "func-sign",
                                  [T_BODY] = "body",
                                  [T_TYPE_REF] = "type-ref",
                                  [T_TYPE_REF_LT] = "type-ref-literal",
                                  [T_BIN_EXPR] = "binary-expression",
                                  [T_BREAK] = "break"

};

struct ast_ident {
  char name[MAXIMUM_IDENTIFIER_LENGTH];
  struct ast_node *type;
};

struct ast_typeref {
  enum type_ref type;
  struct ast_node *sub_field;
};

struct ast_typeref_lt {
  enum literal_type type;
  struct ast_node *sub_field;
};

struct ast_literal {
  union {
    char *value;
  };
  struct ast_node *type;
};

struct ast_node;

struct ast_expression {
  struct ast_node *some_node;
};

struct ast_unary_expression {
  enum operation_type op;
  struct ast_node *argument;
};

struct ast_binary_expression {
  enum operation_type op;
  struct ast_node *arg1;
  struct ast_node *arg2;
};

struct ast_branch {
  struct ast_node *test;
  struct ast_node *consequent;
  struct ast_node *alternate;
};

struct ast_repeat {
  struct ast_node *test;
  struct ast_node *body;
};

struct ast_list {
  struct ast_node *current;
  struct ast_node *next;
};

struct ast_program {
  struct ast_node *child;
};

struct ast_indexer {
  struct ast_node *expr;
  struct ast_node *expr_list;
};

struct ast_func {
  struct ast_node *func_sign;
  struct ast_node *body;
};
struct ast_func_sign {
  struct ast_node *ident;
  struct ast_node *arg_list;
};
struct ast_body {
  struct ast_node *var_list;
  struct ast_node *statement;
};

struct ast_statement {
  struct ast_node *some_node;
};

struct ast_call_expr {
  struct ast_node *expr;
  struct ast_node *expr_list;
};

struct ast_node {
  enum ast_node_type type;
  bool is_visited;
  union {
    struct ast_ident as_ident;
    struct ast_literal as_literal;
    struct ast_expression as_expr;
    struct ast_unary_expression as_unexpr;
    struct ast_binary_expression as_binexpr;
    struct ast_branch as_branch;
    struct ast_repeat as_repeat;
    struct ast_list as_list;
    struct ast_program as_program;
    struct ast_indexer as_indexer;
    struct ast_func as_func;
    struct ast_func_sign as_func_sign;
    struct ast_body as_body;
    struct ast_typeref as_typeref;
    struct ast_typeref_lt as_typeref_lt;
    struct ast_statement as_statement;
    struct ast_call_expr as_call_expr;
  };
};

struct ast_node *make_typeref_array(struct ast_node *);

struct ast_node *make_typeref_ident(struct ast_node *);

struct ast_node *make_typeref_lt(enum literal_type type);

struct ast_node *make_typeref(enum type_ref);

struct ast_node *make_body(struct ast_node *, struct ast_node *);

struct ast_node *make_ident(const char *, struct ast_node *);

struct ast_node *make_func(struct ast_node *, struct ast_node *);

struct ast_node *make_func_sign(struct ast_node *, struct ast_node *);

struct ast_node *set_type_array(struct ast_node *, struct ast_node *);

struct ast_node *make_break();

struct ast_node *make_literal(const char *, struct ast_node *); 

struct ast_node *make_literal_list(struct ast_node *, struct ast_node *);

struct ast_node *insert_literal_list(struct ast_node **, struct ast_node *);

struct ast_node *make_expr(struct ast_node *);

struct ast_node *make_unexpr(enum operation_type, struct ast_node *);

struct ast_node *make_binexpr(enum operation_type, struct ast_node *,
                              struct ast_node *);

struct ast_node *make_expr_call(struct ast_node *, struct ast_node *);

struct ast_node *make_expr_indexer(struct ast_node *, struct ast_node *);

struct ast_node *make_branch(struct ast_node *, struct ast_node *, struct ast_node *);

struct ast_node *make_while(struct ast_node *, struct ast_node *);

struct ast_node *make_repeat(struct ast_node *, struct ast_node *);

struct ast_node *make_expr_indexer(struct ast_node *, struct ast_node *);

struct ast_node *insert_stat_list(struct ast_node **, struct ast_node *);

struct ast_node *make_stat_list(struct ast_node *, struct ast_node *);

struct ast_node *make_statement(struct ast_node *);

struct ast_node *make_argdef_list(struct ast_node *, struct ast_node *);

struct ast_node *insert_argdef_list(struct ast_node **, struct ast_node *);

struct ast_node *make_array(struct ast_node *, struct ast_node *);

struct ast_node *insert_array(struct ast_node **, struct ast_node *);

struct ast_node *insert_expr_list(struct ast_node **, struct ast_node *);

struct ast_node *make_expr_list(struct ast_node *, struct ast_node *);

struct ast_node *make_program(struct ast_node *);

void free_ast(struct ast_node *);

int print_ast(struct ast_node *);

typedef void(process_cb)(struct ast_node *);
