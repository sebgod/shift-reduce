ifeq ($(OS),Windows_NT)
    MAIN_TARGET:=ansicengine.exe
else
    MAIN_TARGET:=ansicengine
endif
CC:=gcc
OBJ:=o
FLAGS:=
CFLAGS:=
CFLAGS+=$(FLAGS)
LD:=$(CC)
LDFLAGS:=
LDFLAGS+=$(FLAGS)
LDOUTPREFIX:=-o

.PHONY: default
default: $(MAIN_TARGET)

$(MAIN_TARGET): ansicengine.$(OBJ) parser.$(OBJ)
	$(LD) $(LDFLAGS) $(LDOUTPREFIX)$@ $^

%.$(OBJ): %.c
	$(CC) $(CFLAGS) -c $<

.PHONY:
clean:
	rm -f $(MAIN_TARGET)
	rm -f $(wildcard *.$(OBJ))
