include ../Make.options

MODULES:=$(wildcard *.m) $(wildcard ../src/*.m)

.PHONY: runtests
runtests: test_egt

.PHONY: runtests-verbose
runtests-verbose: $(TESTS)

Mercury.modules: $(MODULES)
	$(MMC) -f $(MODULES)

tags: $(MODULES)
	$(MTAGS) $(MODULES)

test_egt: Mercury.modules
	$(MMC) $(MCFLAGS) -m $@

.PHONY: realclean
realclean:
	$(MMC) --make test_egt.realclean
	rm -rf Mercury
	rm -f Mercury.modules
