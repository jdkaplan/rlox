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
PROFILE = release
RUSTFLAGS = --release
CDEBUG =
else
PROFILE = debug
RUSTFLAGS =
CDEBUG = -g3
endif

ifeq ($(DEBUG_STRESS),)
CDEBUG_STRESS =
RLOX_STRESS =
else
CDEBUG_STRESS = -DDEBUG_STRESS_GC
RLOX_STRESS = stress_gc
endif

ifeq ($(DEBUG_TRACE),)
CDEBUG_TRACE =
RLOX_TRACE =
else
CDEBUG_TRACE = -DDEBUG_TRACE_EXECUTION \
			   -DDEBUG_PRINT_CODE \
			   -DDEBUG_LOG_GC
RLOX_TRACE = trace_execution \
			 print_code \
			 log_gc
endif

CC     = gcc
CFLAGS = $(CDEBUG) \
		 $(CDEBUG_STRESS) \
		 $(CDEBUG_TRACE) \
		 \
		 -I./rlox \
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
CLOX_FILES = $(wildcard src/*.c) $(wildcard src/*.h) rlox/target/$(PROFILE)/librlox.a

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
clox: $(CLOX_FILES)
	$(MKDIR) "$(BUILD_DIR)"
	$(CC) -o "$(BUILD_DIR)/clox" $(CFLAGS) $(CLOX_FILES)

RLOX_FILES = rlox/Cargo.toml rlox/Cargo.lock $(wildcard rlox/src/*.rs)

.PHONY: rlox
rlox: $(RLOX_FILES)
	cd rlox && cargo build $(RUSTFLAGS) --features '$(RLOX_STRESS) $(RLOX_TRACE)'

rlox/target/$(PROFILE)/librlox.a: rlox

.PHONY: clean
clean:
	$(RM) "$(BUILD_DIR)/clox"
