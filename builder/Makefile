VERSION:=5.2.0
FOLDER_NAME:=GOLD-Builder-$(VERSION)-Cmd

ROOT:=$(shell pwd)
FOLDER:=$(ROOT)/$(FOLDER_NAME)
ZIP_FILE_NAME:=$(FOLDER_NAME).zip
ZIP_FILE=$(ROOT)/$(ZIP_FILE_NAME)

UNZIP:=unzip
BUILD_EXE:=$(FOLDER)/GOLDbuild.exe
WGET:=wget

ifeq ($(OS),Windows_NT)
    BUILD:=$(BUILD_EXE)
else
    BUILD:=$(MONO) $(BUILD)
endif

$(FOLDER): $(ZIP_FILE)
	@for zip in $^ ; do $(UNZIP) $$zip ; done

$(ZIP_FILE):
	@$(WGET) http://www.goldparser.org/builder/files/$(ZIP_FILE_NAME)

.PHONY: clean
clean:
	@rm -f $(ZIP_FILE)
	@rm -fR $(FOLDER)

%.egt: %.grm
	$(BUILD) $<
