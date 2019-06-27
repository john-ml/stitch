// Various useful definitions
#ifndef MISC_INCLUDED_H
#define MISC_INCLUDED_H

#define TO_STR(x) #x

// Make ADT constructors
#define MK_SING(ty, ctr, field, e) \
  node_t __res = malloc(sizeof(*__res)); \
  *__res = (struct ty){.is = ctr, .as = {.field = e}}; \
  return __res
#define MK(ty, ctr, field, ...) \
  node_t __res = malloc(sizeof(*__res)); \
  *__res = (struct ty){.is = ctr, .as = {.field = {__VA_ARGS__}}}; \
  return __res

typedef void *any_t;
typedef void (*del_t)(any_t);

void no_del(any_t);

#endif // MISC_INCLUDED_H
