OBJS= \
parse_sexp.o \
print_sexp.o \
eval_sexp.o \
cons.o \
atom_proc.o \
object.o \
environment.o \
primitives.o \
read_sexp.o \
symbol_table.o \
stream.o

CC=cc
CFLAGS=-Wall -g

liutcl: liutcl.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@
liutcl.o: liutcl.c atom_proc.h environment.h types.h parse_sexp.h read_sexp.h eval_sexp.h print_sexp.h stream.h
	$(CC) -c $(CFLAGS) $< -o $@

parse_sexp.o: parse_sexp.c types.h cons.h atom_proc.h
	$(CC) -c $(CFLAGS) $< -o $@
print_sexp.o: print_sexp.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
eval_sexp.o: eval_sexp.c types.h environment.h atom_proc.h primitives.h stream.h
	$(CC) -c $(CFLAGS) $< -o $@
cons.o: cons.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
atom_proc.o: atom_proc.c cons.h object.h types.h symbol_table.h
	$(CC) -c $(CFLAGS) $< -o $@
object.o: object.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
environment.o: environment.c types.h atom_proc.h stream.h
	$(CC) -c $(CFLAGS) $< -o $@
primitives.o: primitives.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
read_sexp.o: read_sexp.c
	$(CC) -c $(CFLAGS) $< -o $@
symbol_table.o: symbol_table.c symtbl.h types.h
	$(CC) -c $(CFLAGS) $< -o $@

stream.o: stream.c types.h object.h atom_proc.h
	$(CC) -c $(CFLAGS) $< -o $@

xread-print: xread-print.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@
xread-print.o: xread-print.c print_sexp.h parse_sexp.h eval_sexp.h
	$(CC) -c $(CFLAGS) $< -o $@

xread_sexp: xread_sexp.o read_sexp.o
	$(CC) $(CFLAGS) $^ -o $@
xread_sexp.o: xread_sexp.c read_sexp.h
	$(CC) -c $(CFLAGS) $< -o $@

xcons: xcons.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@
xcons.o: xcons.c types.h cons.h
	$(CC) -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm *.o xread-print
