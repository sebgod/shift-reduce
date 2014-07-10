MMC=mmc

ifdef GRADE
    MCFLAGS += -s $(GRADE) --no-detect-libgrades
endif

.PHONY: all
all: test_egt

test_%: test_%.m
	$(MMC) $(MCFLAGS) -m $@
