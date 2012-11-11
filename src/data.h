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

/* Comparison */
extern PHEAD(lt_eq);
extern PHEAD(lt_eql);
/* Cons */
extern PHEAD(lt_car);
extern PHEAD(lt_cdr);
extern PHEAD(lt_cons);
extern PHEAD(lt_consp);
extern PHEAD(lt_list);
extern PHEAD(lt_make_list);
extern PHEAD(lt_nth);
extern PHEAD(lt_nthcdr);
extern PHEAD(lt_rplaca);
extern PHEAD(lt_rplacd);
/* Function */
extern PHEAD(lt_funcall);
extern PHEAD(lt_functionp);
/* Package */
extern PHEAD(lt_find_package);
extern PHEAD(lt_package_name);
/* String */
extern PHEAD(lt_char);
extern PHEAD(lt_make_string);
extern PHEAD(lt_stringp);
/* Symbol */
extern PHEAD(lt_intern);
extern PHEAD(lt_keywordp);
extern PHEAD(lt_set);
extern PHEAD(lt_symbol_name);
extern PHEAD(lt_symbol_package);
extern PHEAD(lt_symbol_value);
extern PHEAD(lt_symbolp);
/* Others */
extern PHEAD(lt_special_operator_p);
extern PHEAD(lt_type_of);
extern PHEAD(lt_values);

#endif
