.DEFAULT_GOAL := help
.PHONY: help
help: ## List targets in this Makefile
	@awk '\
		BEGIN { FS = ":$$|:[^#]+|:.*?## "; OFS="\t" }; \
		/^[0-9a-zA-Z_-]+?:/ { print $$1, $$2 } \
	' $(MAKEFILE_LIST) \
		| sort --dictionary-order \
		| column --separator $$'\t' --table --table-wrap 2 --output-separator '    '

ifeq ($(DEBUG),)
CDEBUG =
else
CDEBUG = -DDEBUG_TRACE_EXECUTION
endif

CC     = gcc
CFLAGS = $(CDEBUG) \
		 -Wall \
		 -Wextra \
		 \
		 -Waggregate-return \
		 -Wcast-align \
		 -Wcast-qual \
		 -Wconversion \
		 -Wfloat-equal \
		 -Wpointer-arith \
		 -Wshadow \
		 -Wstrict-overflow=4 \
		 -Wstrict-prototypes \
		 -Wswitch-default \
		 -Wswitch-enum \
		 -Wundef \
		 -Wunreachable-code \
		 -Wwrite-strings

RM    = rm -f
MKDIR = mkdir -p

BUILD_DIR = ./build
SOURCE_FILES = $(wildcard src/*.c) $(wildcard src/*.h)

.PHONY: build
build: clox

.PHONY: run
run: build
	$(BUILD_DIR)/clox

.PHONY: watch
watch:
	until fd . | entr -cdp $(MAKE) run ; do echo 'Edit a file or press Space to start.'; done

.PHONY: clox
clox: $(SOURCE_FILES)
	$(MKDIR) "$(BUILD_DIR)"
	$(CC) -o "$(BUILD_DIR)/clox" $(CFLAGS) $(SOURCE_FILES)

clean:
	$(RM) "$(BUILD_DIR)/clox"
