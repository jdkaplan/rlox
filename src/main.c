#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"

#include "rlox.h"

void repl(void) {
  hello();

  Vm vm;
  vm_init(&vm);

  char line[1024];
  for (;;) {
    printf("> ");

    if (!fgets(line, sizeof(line), stdin)) {
      printf("\n");
      break;
    }

    vm_interpret(&vm, line);
  }

  vm_free(&vm);
}

static char *read_file(const char *path) {
  FILE *f = fopen(path, "rb");
  if (f == NULL) {
    fprintf(stderr, "could not open file: \"%s\"", path);
    exit(74);
  }

  fseek(f, 0L, SEEK_END);
  size_t size = (size_t)(ftell(f));
  rewind(f);

  char *buf = (char *)malloc(size + 1);
  if (buf == NULL) {
    fprintf(stderr, "not enough memory to read: \"%s\"", path);
    exit(74);
  }

  size_t n = fread(buf, sizeof(char), size, f);
  if (n < size) {
    fprintf(stderr, "could not read file: \"%s\"", path);
    exit(74);
  }

  buf[n] = '\0';

  fclose(f);
  return buf;
}

void run_file(const char *path) {
  Vm vm;
  vm_init(&vm);

  char *source = read_file(path);
  InterpretResult result = vm_interpret(&vm, source);
  free(source);

  switch (result) {
  case INTERPRET_OK:
    break;
  case INTERPRET_COMPILE_ERROR:
    exit(65);
  case INTERPRET_RUNTIME_ERROR:
    exit(70);
  }

  vm_free(&vm);
}

int main(int argc, const char *argv[]) {
  if (argc == 1) {
    repl();
  } else if (argc == 2) {
    run_file(argv[1]);
  } else {
    fprintf(stderr, "Usage: %s [path]\n", argv[0]);
    exit(64);
  }
  return 0;
}
