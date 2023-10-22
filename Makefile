.DEFAULT_GOAL := help
.PHONY: help
help: ## List targets in this Makefile
	@awk '\
		BEGIN { FS = ":$$|:[^#]+|:.*?## "; OFS="\t" }; \
		/^[0-9a-zA-Z_-]+?:/ { print $$1, $$2 } \
	' $(MAKEFILE_LIST) \
		| sort --dictionary-order \
		| column --separator $$'\t' --table --table-wrap 2 --output-separator '    '

CC     = gcc
CFLAGS =

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
	until fd . | entr -cdp $(MAKE) run ; do echo 'Edit a file or press Space to build and run.'; done

.PHONY: clox
clox: $(SOURCE_FILES)
	$(MKDIR) "$(BUILD_DIR)"
	$(CC) -o "$(BUILD_DIR)/clox" $(CFLAGS) $(SOURCE_FILES)

clean:
	$(RM) "$(BUILD_DIR)/clox"
