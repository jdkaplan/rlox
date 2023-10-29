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
CDEBUG = -g3 -DDEBUG_TRACE_EXECUTION -DDEBUG_PRINT_CODE
endif

CC     = gcc
CFLAGS = $(CDEBUG) \
		 -Wall \
		 -Wextra \
		 \
		 -Wcast-align \
		 -Wcast-qual \
		 -Wconversion \
		 -Wfloat-equal \
		 -Wpointer-arith \
		 -Wshadow \
		 -Wstrict-overflow=4 \
		 -Wstrict-prototypes \
		 -Wundef \
		 -Wunreachable-code \
		 -Wwrite-strings

RM    = rm -f
MKDIR = mkdir -p

BUILD_DIR = ./build
SOURCE_FILES = $(wildcard src/*.c) $(wildcard src/*.h)

WATCH = build

.PHONY: build
build: clox

.PHONY: run
run: build
	$(BUILD_DIR)/clox $(RUN_ARGS)

.PHONY: watch
watch:
	until fd . | entr -cdp $(MAKE) $(WATCH) ; do echo 'Edit a file or press Space to start.'; done

.PHONY: clox
clox: $(SOURCE_FILES)
	$(MKDIR) "$(BUILD_DIR)"
	$(CC) -o "$(BUILD_DIR)/clox" $(CFLAGS) $(SOURCE_FILES)

.PHONY: clean
clean:
	$(RM) "$(BUILD_DIR)/clox"
