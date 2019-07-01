// Various useful definitions
#ifndef MISC_INCLUDED_H
#define MISC_INCLUDED_H

#define TO_STR(x) #x

#define MIN(x, y) (x) < (y) ? (x) : (y)
#define MAX(x, y) (x) < (y) ? (y) : (x)

typedef void *any_t;
typedef void (*del_t)(any_t);
typedef int (*cmp_t)(any_t, any_t);

void no_del(any_t);

#endif // MISC_INCLUDED_H
