#include "ast.h"
#include "interning.h"
#include "lexer.h"
#include "parser.tab.h"
#include "typing.h"
#include "arena.h"
#include <stdio.h>

void main(int argc, char **argv) {
  yyin = argc > 1 ? fopen(argv[1], "r") : stdin;
  node_p e;
  arena_p a = arena_new();
  stab_t stab = stab_new(&a);

  yyparse(&e, &stab, &a);
  node_pp(stab, stdout, e);
  printf("vec_len(stab.strs) = %lu\n", vec_len(stab.strs));
  for (int i = 0; (size_t)i < vec_len(stab.strs); ++i)
    printf("stab.strs[%d] = %s\n", i, (char *)stab.strs[i]);
  //check_types(stab, e);

  arena_del(a);
  if (yyin != stdin)
    fclose(yyin);
  yylex_destroy();
}
