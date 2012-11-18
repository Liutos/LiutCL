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
number.o \
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

atom.o: cons.h object.h types.h

cons.o: cons.c object.h package.h stream.h types.h

data.o: data.c atom.h cons.h eval_sexp.h macro_def.h package.h symbol.h types.h

environment.o: environment.c hash_table.h print_sexp.h types.h

eval_sexp.o: eval_sexp.c environment.h function.h macro_def.h stream.h types.h

function.o: function.c decls.h types.h

hash_table.o: hash_table.c object.h types.h

init.o: init.c atom.h arit.h data.h environment.h io.h logic.h macro_def.h stream.h types.h

io.o: io.c atom.h cons.h stream.h types.h

logic.o: logic.c pdecls.h types.h

number.o: object.h types.h

object.o: object.c types.h

package.o: package.c object.h types.h

parse_sexp.o: parse_sexp.c atom.h cons.h object.h types.h

print_sexp.o: print_sexp.c atom.h object.h stream.h types.h

spec.o: spec.c cons.h macro_def.h pdecls.h types.h

read_sexp.o: read_sexp.c

stream.o: stream.c types.h object.h atom.h

symbol.o: symbol.c hash_table.h types.h

vm_stack.o: vm_stack.c atom.h stream.h types.h

# Executables
liutcl.o: liutcl.c eval_sexp.h environment.h init.h parse_sexp.h print_sexp.h read_sexp.h stream.h types.h

bin/liutcl: liutcl.o $(OBJS)
	$(CC) $(CFLAGS) -lgmp $^ -o $@

.PHONY: clean
clean:
	rm *.o
