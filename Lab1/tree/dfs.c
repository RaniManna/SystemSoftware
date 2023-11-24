#include "Dfs.h"
#include <stddef.h>
#include <stdio.h>

void dfs_bypass(struct ast_node *node, process_cb preproccess_cb,
                process_cb postprocess_cb) {
  if (node == NULL)
    return;

  node->is_visited = true;
  preproccess_cb(node);
  switch (node->type) {
  case T_PROGRAM: {
    dfs_bypass(node->as_program.child, preproccess_cb, postprocess_cb);
    break;
  }
  case T_EXPR_LIST:
  case T_ARGDEF_LIST:
  case T_ARRAY:
  case T_LIT_LIST:
  case T_STMTS_LIST: {
    struct ast_node *iter = node;
    while (iter != NULL) {
      dfs_bypass(iter->as_list.current, preproccess_cb, postprocess_cb);
      struct ast_node *temp = iter;
      iter = iter->as_list.next;
    }
    return;
  }
  case T_WHILE:
  case T_REPEAT: {
    dfs_bypass(node->as_repeat.test, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_repeat.body, preproccess_cb, postprocess_cb);
    break;
  }
  case T_BRANCH: {
    dfs_bypass(node->as_branch.test, preproccess_cb,
               postprocess_cb);
    dfs_bypass(node->as_branch.consequent, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_branch.alternate, preproccess_cb, postprocess_cb);
    break;
  }
  case T_BIN_EXPR: {
    dfs_bypass(node->as_binexpr.arg1, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_binexpr.arg2, preproccess_cb, postprocess_cb);

    break;
  }
  case T_UN_EXPR: {
    dfs_bypass(node->as_unexpr.argument, preproccess_cb, postprocess_cb);
    break;
  }
  case T_INDEXER: {
    dfs_bypass(node->as_indexer.expr, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_indexer.expr_list, preproccess_cb, postprocess_cb);
    break;
  }
  case T_CALL_EXPR: {
    dfs_bypass(node->as_call_expr.expr, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_call_expr.expr_list, preproccess_cb, postprocess_cb);
    break;
  }
  case T_FUNC_SIGN: {
    dfs_bypass(node->as_func_sign.ident, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_func_sign.arg_list, preproccess_cb, postprocess_cb);
    break;
  }
  case T_FUNC: {
    dfs_bypass(node->as_func.func_sign, preproccess_cb, postprocess_cb);
    dfs_bypass(node->as_func.body, preproccess_cb, postprocess_cb);
    break;
  }
  case T_IDENT: {
    dfs_bypass(node->as_ident.type, preproccess_cb, postprocess_cb);
    break;
  }
  case T_BODY: {
    dfs_bypass(node->as_body.var_list, preproccess_cb,
               postprocess_cb);
    dfs_bypass(node->as_body.statement, preproccess_cb, postprocess_cb);
    break;
  }

  case T_STMT: {
    dfs_bypass(node->as_statement.some_node, preproccess_cb, postprocess_cb);
    break;
  }
  case T_EXPR: {
    dfs_bypass(node->as_expr.some_node, preproccess_cb, postprocess_cb);
    break;
  }
  case T_TYPE_REF: {
    if (node->as_typeref.type == TR_IDENT || node->as_typeref.type == TR_ARR) {
      dfs_bypass(node->as_typeref.sub_field, preproccess_cb, postprocess_cb);
    }
    break;
  }
  default: {
    break;
  }
  }
  postprocess_cb(node);
  return;
}
