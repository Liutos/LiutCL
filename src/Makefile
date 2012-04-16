# lt: main.c model.c print.c read.c eval.c primitives.c
# 	gcc -Wall -g $^ -o $@
lt: main.o eval.o model.o primitives.o print.o read.o
	gcc -Wall -g $^ -o $@
main.o: main.c eval.h model.h primitives.h print.h read.h types.h
	gcc -Wall -g -c main.c -o $@
primitives.o: primitives.c model.h types.h
	gcc -Wall -g -c primitives.c -o $@
eval.o: eval.c model.h types.h
	gcc -Wall -g -c eval.c -o $@
model.o: model.c types.h
	gcc -Wall -g -c model.c -o $@
print.o: print.c types.h
	gcc -Wall -g -c print.c -o $@
read.o: read.c types.h
	gcc -Wall -g -c read.c -o $@
clean:
	-rm lt *.o
.PHONY: clean