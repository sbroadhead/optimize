CC = gcc
CFLAGS = -Wall -O2 -g
LDFLAGS = -llbfgs

OBJ = test_lbfgs.o

all: test_lbfgs

test_lbfgs: $(OBJ)
	$(CC) $(LDFLAGS) -o test_lbfgs $(OBJ)

.c.o:
	$(CC) $(CFLAGS) -c $<
