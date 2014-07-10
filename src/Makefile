include ../Make.options
NAME:=shift_reduce
LIB:=lib$(NAME)
MODULES:=$(wildcard *.m)

.PHONY: default
default:
	$(MMC) $(MCFLAGS) -m $(LIB)

.PHONY: rebuild
rebuild:
	$(MMC) --rebuild $(LIB)

.PHONY: install
install:
	$(MMC) --make $(LIB).install

tags: $(MODULES)
	$(MTAGS) $^

.PHONY: clean
clean:
	$(MMC) --make $(NAME).clean

.PHONY: realclean
realclean:
	$(MMC) --make $(NAME).realclean
	rm -rf Mercury
	rm -f $(wildcard *.err) $(wildcard *.mh)
	rm -f tags
