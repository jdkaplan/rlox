#ifndef rlox_bindings_h
#define rlox_bindings_h

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define U8_COUNT ((uintptr_t)UINT8_MAX + 1)

#define FRAMES_MAX 64

#define STACK_MAX (FRAMES_MAX * U8_COUNT)

typedef enum FunctionMode {
  MODE_FUNCTION,
  MODE_INITIALIZER,
  MODE_METHOD,
  MODE_SCRIPT,
} FunctionMode;

typedef enum InterpretResult {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;

typedef enum ObjType {
  O_BOUND_METHOD,
  O_CLASS,
  O_CLOSURE,
  O_FUNCTION,
  O_INSTANCE,
  O_NATIVE,
  O_STRING,
  O_UPVALUE,
} ObjType;

typedef enum Opcode {
  OP_CONSTANT,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_DEFINE_GLOBAL,
  OP_GET_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_GET_PROPERTY,
  OP_SET_PROPERTY,
  OP_GET_SUPER,
  OP_NOT,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_NEG,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_PRINT,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LOOP,
  OP_CALL,
  OP_INVOKE,
  OP_SUPER_INVOKE,
  OP_CLOSURE,
  OP_CLOSE_UPVALUE,
  OP_RETURN,
  OP_CLASS,
  OP_INHERIT,
  OP_METHOD,
} Opcode;

typedef enum TokenType {
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_COMMA,
  TOKEN_DOT,
  TOKEN_MINUS,
  TOKEN_PLUS,
  TOKEN_SEMICOLON,
  TOKEN_SLASH,
  TOKEN_STAR,
  TOKEN_BANG,
  TOKEN_BANG_EQUAL,
  TOKEN_EQUAL,
  TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER,
  TOKEN_GREATER_EQUAL,
  TOKEN_LESS,
  TOKEN_LESS_EQUAL,
  TOKEN_IDENTIFIER,
  TOKEN_STRING,
  TOKEN_NUMBER,
  TOKEN_AND,
  TOKEN_CLASS,
  TOKEN_ELSE,
  TOKEN_FALSE,
  TOKEN_FOR,
  TOKEN_FUN,
  TOKEN_IF,
  TOKEN_NIL,
  TOKEN_OR,
  TOKEN_PRINT,
  TOKEN_RETURN,
  TOKEN_SUPER,
  TOKEN_THIS,
  TOKEN_TRUE,
  TOKEN_VAR,
  TOKEN_WHILE,
  TOKEN_ERROR,
  TOKEN_EOF,
} TokenType;

typedef enum ValueType {
  T_BOOL,
  T_NIL,
  T_NUMBER,
  T_OBJ,
} ValueType;

typedef struct Scanner {
  const char *start;
  const char *current;
  int line;
} Scanner;

typedef struct Token {
  enum TokenType type;
  const char *start;
  int length;
  int line;
} Token;

typedef struct Obj {
  enum ObjType type;
  bool is_marked;
  struct Obj *next;
} Obj;

typedef union ValueAs {
  bool boolean;
  double number;
  struct Obj *obj;
} ValueAs;

typedef struct Value {
  enum ValueType type;
  union ValueAs as;
} Value;

typedef struct Vec_u8 {
  unsigned int len;
  unsigned int cap;
  uint8_t *items;
} Vec_u8;

typedef struct Vec_u8 Bytecode;

typedef struct Vec_Value {
  unsigned int len;
  unsigned int cap;
  struct Value *items;
} Vec_Value;

typedef struct Vec_Value Values;

typedef struct Vec_c_int {
  unsigned int len;
  unsigned int cap;
  int *items;
} Vec_c_int;

typedef struct Vec_c_int Lines;

typedef struct Chunk {
  Bytecode code;
  Values constants;
  Lines lines;
} Chunk;

typedef struct ObjString {
  struct Obj obj;
  uintptr_t length;
  char *chars;
  uint32_t hash;
} ObjString;

typedef struct ObjFunction {
  struct Obj obj;
  unsigned int arity;
  int upvalue_count;
  struct Chunk chunk;
  struct ObjString *name;
} ObjFunction;

typedef struct ObjUpvalue {
  struct Obj obj;
  struct Value *location;
  struct Value closed;
  struct ObjUpvalue *next;
} ObjUpvalue;

typedef struct ObjClosure {
  struct Obj obj;
  struct ObjFunction *function;
  struct ObjUpvalue **upvalues;
  int upvalue_count;
} ObjClosure;

typedef struct CallFrame {
  struct ObjClosure *closure;
  uint8_t *ip;
  struct Value *slots;
} CallFrame;

typedef struct Entry {
  struct ObjString *key;
  struct Value value;
} Entry;

typedef struct Table {
  unsigned int size;
  unsigned int cap;
  struct Entry *entries;
} Table;

typedef struct Vm {
  struct CallFrame frames[FRAMES_MAX];
  unsigned int frame_count;
  struct Value stack[STACK_MAX];
  struct Value *stack_top;
  struct Table globals;
  struct Table strings;
  struct ObjString *init_string;
  struct ObjUpvalue *open_upvalues;
  struct Obj *objects;
  uintptr_t gc_pending_len;
  uintptr_t gc_pending_cap;
  struct Obj **gc_pending_stack;
  uintptr_t bytes_allocated;
  uintptr_t next_gc;
} Vm;

typedef struct Local {
  struct Token name;
  int depth;
  bool is_captured;
} Local;

typedef struct Upvalue {
  uint8_t index;
  bool is_local;
} Upvalue;

typedef struct Compiler {
  struct Compiler *enclosing;
  struct ObjFunction *function;
  enum FunctionMode mode;
  struct Local locals[U8_COUNT];
  int local_count;
  int scope_depth;
  struct Upvalue upvalues[U8_COUNT];
  int upvalue_count;
} Compiler;

typedef struct Gc {
  struct Vm *vm;
  struct Compiler *compiler;
} Gc;

typedef struct ClassCompiler {
  struct ClassCompiler *enclosing;
  bool has_superclass;
} ClassCompiler;

typedef struct ObjBoundMethod {
  struct Obj obj;
  struct Value receiver;
  struct ObjClosure *method;
} ObjBoundMethod;

typedef struct ObjClass {
  struct Obj obj;
  struct ObjString *name;
  struct Table methods;
} ObjClass;

typedef struct ObjInstance {
  struct Obj obj;
  struct ObjClass *klass;
  struct Table fields;
} ObjInstance;

typedef struct Value (*NativeFn)(unsigned int argc, const struct Value *argv);

typedef struct ObjNative {
  struct Obj obj;
  NativeFn fn;
} ObjNative;

typedef struct Parser {
  struct Token current;
  struct Token previous;
  struct Scanner *scanner;
  struct Compiler *compiler;
  struct ClassCompiler *klass;
  bool had_error;
  bool panicking;
  struct Vm *vm;
} Parser;

void scanner_init(struct Scanner *scanner, const char *source);

struct Token scanner_next(struct Scanner *scanner);

void dbg_token(struct Token token);

void print_value(struct Value value);

void println_value(struct Value value);

bool value_eq(struct Value a, struct Value b);

void chunk_init(struct Chunk *chunk);

void chunk_free(struct Gc gc, struct Chunk *chunk);

void chunk_write(struct Gc gc, struct Chunk *chunk, uint8_t byte, int line);

uint8_t chunk_add_constant(struct Gc gc, struct Chunk *chunk, struct Value value);

void hello(void);

void *reallocate(struct Gc gc, void *ptr, uintptr_t old, uintptr_t new_);

void *_reallocate(void *ptr, uintptr_t new_);

void ___export_all(Bytecode,
                   struct CallFrame,
                   struct Chunk,
                   struct ClassCompiler,
                   struct Compiler,
                   struct Entry,
                   enum FunctionMode,
                   struct Gc,
                   enum InterpretResult,
                   Lines,
                   struct Local,
                   struct Obj,
                   struct ObjBoundMethod,
                   struct ObjClass,
                   struct ObjClosure,
                   struct ObjFunction,
                   struct ObjInstance,
                   struct ObjNative,
                   struct ObjString,
                   enum ObjType,
                   struct ObjUpvalue,
                   enum Opcode,
                   struct Parser,
                   struct Scanner,
                   struct Table,
                   struct Token,
                   enum TokenType,
                   struct Upvalue,
                   struct Value,
                   union ValueAs,
                   Values,
                   enum ValueType,
                   struct Vm);

#endif /* rlox_bindings_h */
