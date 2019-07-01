#include "ast.h"
#include "interning.h"
#include "lexer.h"
#include "parser.tab.h"
#include "typing.h"
#include "arena.h"
#include <stdio.h>

void main(int argc, char **argv) {
  yyin = argc > 1 ? fopen(argv[1], "r") : stdin;
  node_t e;
  stab_t stab = stab_new();
  arena_t a = arena_new();

  yyparse(&e, &stab, &a);
  node_pp(stab, stdout, e);
  //printf("vec_len(stab.strs) = %d\n", vec_len(stab.strs));
  //for (int i = 0; i < vec_len(stab.strs); ++i)
  //  printf("stab.strs[%d] = %s\n", i, (char *)stab.strs[i]);
  //check_types(stab, e);

  stab_del(stab);
  arena_del(a);
  if (yyin != stdin)
    fclose(yyin);
  yylex_destroy();
}
