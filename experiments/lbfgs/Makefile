CC = gcc
CFLAGS = -Wall -O2 -g -I/usr/local/include
LDFLAGS = -L/usr/local/lib -llbfgs

all: test_lbfgs

test_lbfgs: test_lbfgs.o
	$(CC) test_lbfgs.o $(LDFLAGS) -o test_lbfgs 

test_lbfgs.o: dummy
	$(CC) $(CFLAGS) -c test_lbfgs.c

thrust: thrust.o
	$(CC) thrust.o $(LDFLAGS) -o thrust 

thrust.o: dummy
	$(CC) $(CFLAGS) -c thrust.c

dummy:
