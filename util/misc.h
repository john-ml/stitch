// Various useful definitions
#ifndef MISC_INCLUDED_H
#define MISC_INCLUDED_H

#define TO_STR(x) #x

typedef void *any_t;
typedef void (*del_t)(any_t);

void no_del(any_t);

#endif // MISC_INCLUDED_H
