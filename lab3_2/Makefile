CC = gcc
CFLAGS = -Wall -Wextra -g
LEX = flex
YACC = bison
YFLAGS = -d

TARGET = l2formatter
SRCS = main.c
OBJS = main.o l2.tab.o lex.l2.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

main.o: main.c l2.tab.h
	$(CC) $(CFLAGS) -c main.c

l2.tab.c l2.tab.h: l2.y
	$(YACC) $(YFLAGS) l2.y

lex.l2.c: l2.l l2.tab.h
	$(LEX) -o lex.l2.c l2.l

l2.tab.o: l2.tab.c
	$(CC) $(CFLAGS) -c l2.tab.c

lex.l2.o: lex.l2.c l2.tab.h
	$(CC) $(CFLAGS) -c lex.l2.c

clean:
	rm -f $(TARGET) $(OBJS) l2.tab.c l2.tab.h lex.l2.c

.PHONY: all clean 