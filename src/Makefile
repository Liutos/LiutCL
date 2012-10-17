OBJS=parse_sexp.o print_sexp.o eval_sexp.o cons.o atom_proc.o object.o environment.o primitives.o read_sexp.o symbol_table.o

liutcl: liutcl.o $(OBJS)
	gcc -Wall -g $^ -o $@
liutcl.o: liutcl.c atom_proc.h environment.h types.h parse_sexp.h read_sexp.h eval_sexp.h print_sexp.h
	gcc -c -Wall -g $< -o $@

parse_sexp.o: parse_sexp.c types.h cons.h atom_proc.h
	gcc -c -Wall -g $< -o $@
print_sexp.o: print_sexp.c types.h
	gcc -c -Wall -g $< -o $@
eval_sexp.o: eval_sexp.c types.h environment.h atom_proc.h primitives.h
	gcc -c -Wall -g $< -o $@
cons.o: cons.c types.h
	gcc -c -Wall -g $< -o $@
atom_proc.o: atom_proc.c cons.h object.h types.h symbol_table.h
	gcc -c -Wall -g $< -o $@
object.o: object.c types.h
	gcc -c -Wall -g $< -o $@
environment.o: environment.c types.h atom_proc.h
	gcc -c -Wall -g $< -o $@
primitives.o: primitives.c types.h
	gcc -c -Wall -g $< -o $@
read_sexp.o: read_sexp.c
	gcc -c -Wall -g $< -o $@
symbol_table.o: symbol_table.c symtbl.h
	gcc -c -Wall -g $< -o $@

xread-print: xread-print.o $(OBJS)
	gcc -Wall -g $^ -o $@
xread-print.o: xread-print.c print_sexp.h parse_sexp.h eval_sexp.h
	gcc -c -Wall -g $< -o $@

xread_sexp: xread_sexp.o read_sexp.o
	gcc -Wall -g $^ -o $@
xread_sexp.o: xread_sexp.c read_sexp.h
	gcc -c -Wall -g $< -o $@

.PHONY: clean
clean:
	rm *.o xread-print