#include "ast.h"

void dfs_bypass(struct ast_node *node, process_cb preproccess_cb,
                process_cb postprocess_cb);
