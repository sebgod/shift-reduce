MMC=mmc

ifdef GRADE
    MCFLAGS += -s $(GRADE) --no-detect-libgrades
endif

.PHONY: libshift_reduce
libshift_reduce:
	$(MMC) $(MCFLAGS) -m $@

.PHONY: test

test: libshift_reduce
	cd ../test && $(MAKE)
