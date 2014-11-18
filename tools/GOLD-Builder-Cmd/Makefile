VERSION:=5.2.0
FOLDER_NAME:=GOLD-Builder-$(VERSION)-Cmd

ROOT:=$(shell pwd)
FOLDER:=$(ROOT)/$(FOLDER_NAME)
ZIP_FILE_NAME:=$(FOLDER_NAME).zip
ZIP_FILE=$(ROOT)/$(ZIP_FILE_NAME)

UNZIP:=unzip
WGET:=wget

ifeq ($(OS),Windows_NT)
    BUILD:=$(FOLDER)/GOLDbuild.exe
else
    CSHARP_COMPILER:=$(shell mmc --output-csharp-compiler-type)
    BUILD:=$(CSHARP_COMPILER) $(FOLDER)/GOLDbuild
endif

.PHONY: default
default: $(FOLDER)

$(FOLDER): $(ZIP_FILE)
	@cd "$(ROOT)" && $(UNZIP) $(ZIP_FILE_NAME)
	@cp "$(ROOT)/GOLDbuild-$(VERSION).unix" "$(FOLDER)/GOLDbuild"

$(ZIP_FILE):
	$(WGET) -P "$(ROOT)" http://www.goldparser.org/builder/files/$(ZIP_FILE_NAME)

.PHONY: clean
clean:
	@rm -f $(ZIP_FILE)
	@rm -fR $(FOLDER)

%.egt: %.grm
	@cd $(patsubst %\,%,$(dir $<)) && $(BUILD) $(notdir $<)
