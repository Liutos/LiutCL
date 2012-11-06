#ifndef INIT_H
#define INIT_H

#include "env_types.h"

extern Environment init_dvars(Environment);
extern Environment init_primitives(Environment);
extern Environment init_variables(Environment);
extern void init_special_operators(void);

#endif
