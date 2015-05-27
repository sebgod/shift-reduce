include ../Make.options
include ../Project.options

TEST_MODULES:=$(wildcard *.m)
MODULES:=$(TEST_MODULES) $(wildcard ../src/*.m)
EXPS:=$(wildcard *.exp)
TESTS:=$(patsubst test_%.m,test_%,$(TEST_MODULES))
DIFFS:=$(patsubst %.exp,%.diff,$(EXPS))
RESS:=$(patsubst %.exp,%.res,$(EXPS))

.PHONY: runtests
runtests:  compile $(RESS)

%.res %.diff: %.exp
	@./run_test.sh "test_$(PROJ_CNAME)" "$*" >"$@" 2>"$*.test_err" || true
	@$(DIFF) $(DIFF_FLAGS) $*.exp $@ 1>$*.diff || true

Mercury.modules: $(MODULES)
	@$(MMC) -f $^
	@echo cleaning up for testing
	rm -f $(RESS)

tags: $(MODULES)
	$(MTAGS) $^

.PHONY: compile
compile: Mercury.modules
	@$(foreach test,$(TESTS),$(MMC) --make $(test) --ml mercury_misc;)

.PHONY: cleantemp
cleantemp:
	rm -f $(DIFFS)
	rm -f *.log
	rm -f *.test_err
	rm -f $(RESS)

.PHONY: clean
clean: cleantemp
	@$(foreach test,$(TESTS),$(MMC) --make $(test).clean;)

.PHONY: realclean
realclean: cleantemp
	@$(foreach test,$(TESTS),$(MMC) --make $(test).realclean;)
	rm -rf Mercury
	rm -f Mercury.modules
	rm -f *.mh *.err
