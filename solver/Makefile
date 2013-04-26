CC = gcc
CFLAGS = -Wall -O0 -g -I/usr/local/include
LDFLAGS = -L/usr/local/lib -llbfgs

all: solver

SOLVER_OBJ = main.o region.o util.o

solver: $(SOLVER_OBJ)
	$(CC) $(SOLVER_OBJ) $(LDFLAGS) -o solver

.c.o:
	$(CC) $(CFLAGS) -c $<

