include ../Make.options
include ../Project.options

MODULES:=$(wildcard *.m) $(wildcard ../src/*.m)

.PHONY: runtests
runtests: test_$(PROJ_CNAME)

.PHONY: runtests-verbose
runtests-verbose: $(TESTS)

Mercury.modules: $(MODULES)
	$(MMC) -f $^

tags: $(MODULES)
	$(MTAGS) $^

test_$(PROJ_CNAME): Mercury.modules
	$(MMC) $(MCFLAGS) -m $@

.PHONY: realclean
realclean:
	$(MMC) --make test_$(PROJ_CNAME).realclean
	rm -rf Mercury
	rm -f Mercury.modules