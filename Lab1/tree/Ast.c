#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Dfs.h"

extern int is_verbose;
static int nr_spaces = 0;

const char *ot_symbol[] = {
    [OT_BIN_PLUS] = "+",  [OT_MINUS] = "-",

    [OT_BIN_MUL] = "*",   [OT_BIN_DIV] = "/",

    [OT_BIN_LESS] = "=<", [OT_BIN_GREATER] = ">=", [OT_BIN_EQUALS] = "==",

    [OT_ASSIGN] = ":=",

    [OT_UN_NOT] = "not"};

static struct ast_node *make_node(enum ast_node_type type) {
  struct ast_node *node = (struct ast_node *)malloc(sizeof(struct ast_node));
  if (node == NULL) {
    fprintf(stdout, "%s: MEMORY ALLOCATION ERR\n", __func__);
    exit(1);
  }
  node->type = type;
  node->is_visited = false;
  is_verbose &&fprintf(stdout, "%s: ", ant_names[type]);
  return node;
}

struct ast_node *make_typeref_array(struct ast_node *node_arr) {
  struct ast_node *node = make_node(T_TYPE_REF);
  node->as_typeref.type = TR_ARR;
  node->as_typeref.sub_field = node_arr;
  return node;
}

struct ast_node *make_typeref_ident(struct ast_node *node_ident) {
  struct ast_node *node = make_node(T_TYPE_REF);
  node->as_typeref.type = TR_IDENT;
  node->as_typeref.sub_field = node_ident;
  return node;
}

struct ast_node *make_typeref(enum type_ref type) {
  struct ast_node *node = make_node(T_TYPE_REF);
  node->as_typeref.type = type;
  return node;
}

struct ast_node *make_typeref_lt(enum literal_type type) {
  struct ast_node *node = make_node(T_TYPE_REF_LT);
  node->as_typeref_lt.type = type;
  return node;
}

struct ast_node *make_ident(const char *name, struct ast_node *typeref_node) {
  struct ast_node *ident = make_node(T_IDENT);
  strncpy(ident->as_ident.name, name, MAXIMUM_IDENTIFIER_LENGTH);
  ident->as_ident.type = typeref_node;
  is_verbose &&fprintf(stdout, "[name: %s]\n", name);
  return ident;
}

struct ast_node *make_func_sign(struct ast_node *name,
                                struct ast_node *arg_list) {
  struct ast_node *fs = make_node(T_FUNC_SIGN);
  fs->as_func_sign.ident = name;
  fs->as_func_sign.arg_list = arg_list;
  return fs;
}

struct ast_node *make_func(struct ast_node *sign, struct ast_node *body) {
  struct ast_node *func = make_node(T_FUNC);
  func->as_func.func_sign = sign;
  func->as_func.body = body;
  return func;
}

struct ast_node *make_body(struct ast_node *var_list,
                           struct ast_node *statement) {
  struct ast_node *body = make_node(T_BODY);
  body->as_body.var_list = var_list;
  body->as_body.statement = statement;
  return body;
}

struct ast_node *set_type_array(struct ast_node *node, struct ast_node *type) {
  struct ast_node *iter = node->as_list.current;
  return node;
}

struct ast_node *make_literal(const char *value, struct ast_node *lt) {
  struct ast_node *constant = make_node(T_LITERAL);
  constant->as_literal.type = lt;
  constant->as_literal.value = value;
  is_verbose &&fprintf(stdout, "[op: %s]\n", value);
  return constant;
}

struct ast_node *make_expr(struct ast_node *node) {
  struct ast_node *expr = make_node(T_EXPR);
  expr->as_expr.some_node = node;
  return expr;
}

struct ast_node *make_unexpr(enum operation_type op, struct ast_node *arg) {
  struct ast_node *unexpr = make_node(T_UN_EXPR);

  unexpr->as_unexpr.op = op;
  unexpr->as_unexpr.argument = arg;
  is_verbose &&fprintf(stdout, "[op: %s]\n", ot_symbol[op]);
  return unexpr;
}

struct ast_node *make_binexpr(enum operation_type op, struct ast_node *arg1,
                              struct ast_node *arg2) {
  struct ast_node *binexpr = make_node(T_BIN_EXPR);
  binexpr->as_binexpr.op = op;
  binexpr->as_binexpr.arg1 = arg1;
  binexpr->as_binexpr.arg2 = arg2;
  return binexpr;
}

struct ast_node *make_expr_call(struct ast_node *expr,
                                struct ast_node *expr_list) {
  struct ast_node *node = make_node(T_CALL_EXPR);
  node->as_call_expr.expr = expr;
  node->as_call_expr.expr_list = expr_list;
  return expr;
}

struct ast_node *make_break() {
  struct ast_node *node = make_node(T_BREAK);
  return node;
}

struct ast_node *make_branch(struct ast_node *test, struct ast_node *consequent,
                             struct ast_node *alternate) {
  struct ast_node *br = make_node(T_BRANCH);
  br->as_branch.test = test;
  br->as_branch.consequent = consequent;
  br->as_branch.alternate = alternate;
  //br->as_branch.alternate
  return br;
}

struct ast_node *make_while(struct ast_node *test, struct ast_node *body) {
  struct ast_node *wh = make_node(T_WHILE);
  wh->as_repeat.test = test;
  wh->as_repeat.body = body;
  return wh;
}

struct ast_node *make_repeat(struct ast_node *test, struct ast_node *body) {
  struct ast_node *repeat = make_node(T_REPEAT);
  repeat->as_repeat.test = test;
  repeat->as_repeat.body = body;
  return repeat;
}

struct ast_node *make_expr_indexer(struct ast_node *expr,
                                   struct ast_node *expr_list) {
  struct ast_node *indx = make_node(T_INDEXER);
  indx->as_indexer.expr = expr;
  indx->as_indexer.expr_list = expr_list;
  return indx;
}

#define MAKE_LIST(type, head, next)                                            \
  struct ast_node *list = make_node(type);                                     \
  list->as_list.current = head;                                                \
  list->as_list.next = next;                                                   \
  return list;

#define INSERT_LIST(head_p, first, func)                                       \
  if (*head_p) {                                                               \
    struct ast_node *const oldstart =                                          \
        func((*head_p)->as_list.current, (*head_p)->as_list.next);             \
    free(*head_p);                                                             \
    (*head_p) = func(first, oldstart);                                         \
  } else                                                                       \
    *head_p = func(first, NULL);                                               \
  return *head_p;

struct ast_node *insert_stat_list(struct ast_node **head_p, 
                                  struct ast_node *first) {
  INSERT_LIST(head_p, first, make_stat_list);
}

struct ast_node *make_stat_list(struct ast_node *head, struct ast_node *next) {
  MAKE_LIST(T_STMTS_LIST, head, next);
}

struct ast_node *make_statement(struct ast_node *node) {
  struct ast_node *stat = make_node(T_STMT);
  stat->as_statement.some_node = node;
  return stat;
}

struct ast_node *make_argdef_list(struct ast_node *head,
                                  struct ast_node *next) {
  MAKE_LIST(T_ARGDEF_LIST, head, next);
}

struct ast_node *insert_argdef_list(struct ast_node **head_p,
                                    struct ast_node *first) {
  INSERT_LIST(head_p, first, make_argdef_list);
}

struct ast_node *make_array(struct ast_node *head, struct ast_node *next) {
  MAKE_LIST(T_ARRAY, head, next);
}

struct ast_node *insert_array(struct ast_node **head_p,
                              struct ast_node *first) {
  INSERT_LIST(head_p, first, make_array);
}

struct ast_node *insert_expr_list(struct ast_node **head_p,
                                  struct ast_node *first) {
  INSERT_LIST(head_p, first, make_expr_list);
}

struct ast_node *make_expr_list(struct ast_node *head, struct ast_node *next) {
  MAKE_LIST(T_EXPR_LIST, head, next);
}

struct ast_node *make_literal_list(struct ast_node *head,
                                   struct ast_node *next) {
  MAKE_LIST(T_LIT_LIST, head, next);
}

struct ast_node *insert_literal_list(struct ast_node **head_p,
                                     struct ast_node *first) {
  INSERT_LIST(head_p, first, make_literal_list);
}

struct ast_node *make_program(struct ast_node *child) {
  struct ast_node *program = make_node(T_PROGRAM);

  program->as_program.child = child;
  return program;
}

void free_ast(struct ast_node *);

static void postprint_ast() { nr_spaces -= 2; }

static void preprint_ast(struct ast_node *node) {
  nr_spaces += 2;
  if (nr_spaces >= 0) {
    for (size_t i = 0; i < nr_spaces; ++i) {
      fprintf(stdout, " ");
    }
  }
  fprintf(stdout, "%s ", ant_names[node->type]);
  switch (node->type) {
  case T_PROGRAM: {
    fprintf(stdout, "\n");
    break;
  }
  case T_EXPR_LIST:
  case T_ARGDEF_LIST:
  case T_ARRAY:
  case T_LIT_LIST:
  case T_STMTS_LIST: {
    fprintf(stdout, "\n");
    break;
  }
  case T_REPEAT: {
    fprintf(stdout, "\n");
    break;
  }
  case T_LITERAL: {
    fprintf(stdout, "[val: %s, type: %s]\n", node->as_literal.value,
            literal_names[node->as_literal.type->as_typeref.type]);
    break;
  }
  case T_IDENT: {
    fprintf(stdout, "[name: %s]\n", node->as_ident.name);
    break;
  }
  case T_TYPE_REF: {
    fprintf(stdout, "[type: %s]\n", typeref_names[node->as_typeref.type]);

    break;
  }
  case T_INDEXER:
  case T_CALL_EXPR:
  case T_BREAK:
  case T_EXPR:
  case T_WHILE:
  case T_BRANCH:
  case T_STMT:
  case T_BODY:
  case T_FUNC:
  case T_FUNC_SIGN: {
    fprintf(stdout, "\n");
    break;
  }
  case T_UN_EXPR: {
    fprintf(stdout, "[type: %s]\n", ot_symbol[node->as_unexpr.op]);
    break;
  }
  case T_BIN_EXPR: {
    fprintf(stdout, "[type: %s]\n", ot_symbol[node->as_binexpr.op]);
    break;
  }
  default: {
    fprintf(stdout, "<unknown-node>\n");
    break;
  }
  }
}

int print_ast(struct ast_node *root) {
  dfs_bypass(root, preprint_ast, postprint_ast);
  return 1;
}
