CC= cc
CFLAGS= -Wall -g -std=c99 -D_GNU_SOURCE
CPPFLAGS=

OBJS= \
arit.o \
atom.o \
cons.o \
data.o \
environment.o \
eval_sexp.o \
function.o \
hash_table.o \
init.o \
io.o \
logic.o \
object.o \
package.o \
parse_sexp.o \
print_sexp.o \
read_sexp.o \
spec.o \
stream.o \
symbol.o \
vm_stack.o

# Source files
arit.o: arit.c atom.h cons.h pdecls.h types.h

atom.o: atom.c cons.h object.h types.h

cons.o: cons.c types.h

data.o: data.c eval_sexp.h package.h

environment.o: atom.h environment.c env_types.h print_sexp.h types.h

eval_sexp.o: eval_sexp.c atom.h environment.h stream.h types.h

function.o: function.c types.h

hash_table.o: hash_table.c object.h types.h

init.o: init.c atom.h arit.h data.h environment.h io.h logic.h stream.h types.h

io.o: io.c atom.h cons.h stream.h types.h

logic.o: logic.c atom.h cons.h types.h

object.o: object.c types.h

package.o: package.c object.h types.h

parse_sexp.o: parse_sexp.c atom.h cons.h object.h types.h

print_sexp.o: print_sexp.c atom.h object.h stream.h types.h

spec.o: spec.c cons.h types.h

read_sexp.o: read_sexp.c

stream.o: stream.c types.h object.h atom.h

symbol.o: symbol.c hash_table.h types.h

vm_stack.o: vm_stack.c atom.h stream.h types.h

# Executables
liutcl.o: liutcl.c env_types.h eval_sexp.h environment.h init.h parse_sexp.h print_sexp.h read_sexp.h stream.h types.h

liutcl: liutcl.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

xcons.o: xcons.c types.h cons.h

xcons: xcons.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

xread-print.o: xread-print.c stream.h

xread-print: xread-print.o atom.o cons.o object.o stream.o
	$(CC) $(CFLAGS) $^ -o $@

xread_sexp.o: xread_sexp.c read_sexp.h

xread_sexp: xread_sexp.o read_sexp.o
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm *.o xread-print
