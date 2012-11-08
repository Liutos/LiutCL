/*
 * data.h
 *
 *
 *
 * Copyright (C) 2012-11-07 liutos <mat.liutos@gmail.com>
 */
#ifndef DATA_H
#define DATA_H

#include "pdecls.h"

/* Cons */
extern PHEAD(lt_car);
extern PHEAD(lt_cdr);
extern PHEAD(lt_cons);
extern PHEAD(lt_rplaca);
extern PHEAD(lt_rplacd);
/* Package */
extern PHEAD(lt_find_package);
/* Others */
extern PHEAD(lt_eq);
extern PHEAD(lt_special_operator_p);
extern PHEAD(lt_type_of);

#endif
