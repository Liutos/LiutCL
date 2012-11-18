#ifndef READ_SEXP_H
#define READ_SEXP_H

#include <stdio.h>

extern char *read_sexp(FILE *);
extern void *free_sexp(char *);

#endif
