CC = gcc
CFLAGS = -Wall -O2 -g
LDFLAGS = -llbfgs

all: test_lbfgs

test_lbfgs: test_lbfgs.o
	$(CC) $(LDFLAGS) -o test_lbfgs test_lbfgs.o

test_lbfgs.o: dummy
	$(CC) $(CFLAGS) -c test_lbfgs.c

dummy:
