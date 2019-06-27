#include "ast.h"
#include "interning.h"
#include "lexer.h"
#include "parser.tab.h"
#include <stdio.h>

void main(int argc, char **argv) {
  yyin = argc > 1 ? fopen(argv[1], "r") : stdin;
  node_t e;
  stab_t interned = stab_new();

  yyparse(&e, &interned);
  node_pp(interned, stdout, e);
  printf("vec_len(interned.strs) = %d\n", vec_len(interned.strs));
  for (int i = 0; i < vec_len(interned.strs); ++i)
    printf("interned.strs[%d] = %s\n", i, (char *)interned.strs[i]);

  node_del(e);
  stab_del(interned);
  if (yyin != stdin)
    fclose(yyin);
  yylex_destroy();
}
