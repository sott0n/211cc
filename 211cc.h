#define _GNU_SOURCE
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct Type Type;

//
// tokenize.c
//

// Token
typedef enum {
    TK_RESERVED,    // Keywords or puctuators
    TK_NUM,         // Integer literals
    TK_IDENT,       // Identifiers
    TK_EOF,         // End of file
} TokenKind;

// Token type
typedef struct Token Token;
struct Token {
    TokenKind kind; // token type
    Token *next;    // next token
    long val;       // if ty is TK_NUM, val is number.
    char *loc;      // token location
    int len;        // token length
};

void error(char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize(char *input);

//
// parse.c
//

// variable
typedef struct Var Var;
struct Var {
    Var *next;      // next Var
    char *name;     // Variable name
    Type *ty;       // Type
    int offset;     // Offset from RBP
    bool is_local;  // local or global
};

// Define a type of AST
typedef enum {
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <
    ND_LE,          // <=
    ND_ASSIGN,      // =
    ND_ADDR,        // unary &
    ND_DEREF,       // unary *
    ND_RETURN,      // "return"
    ND_IF,          // "if"
    ND_FOR,         // "for" or "while"
    ND_BLOCK,       // { ... }
    ND_FUNCALL,     // Function call
    ND_EXPR_STMT,   // Expression statement
    ND_VAR,         // Variable
    ND_NUM,         // integer
} NodeKind;

// Type of node
typedef struct Node Node;
struct Node {
    NodeKind kind;      // Node kind
    Node *next;         // Next node
    Type *ty;           // Type, e.g. int or pointer to int
    Token *tok;         // Representative token
    Node *lhs;          // Left-hand side
    Node *rhs;          // Right-hand side

    // "if" or "for" statement
    Node *cond;
    Node *then;
    Node *els;
    Node *init;
    Node *inc;

    // Block
    Node *body;

    // Function call
    char *funcname;
    Node *args;

    Var *var;           // Use it if only kind == ND_VAR
    long val;           // Use it if only kind == ND_NUM
};

typedef struct Function Function;
struct Function {
    Function *next;
    char *name;
    Var *params;

    Node *node;
    Var *locals;
    int stack_size;
};

typedef struct {
    Var *globals;
    Function *fns;
} Program;

Program *parse(Token *tok);

//
// typing.c
//

typedef enum { TY_INT, TY_PTR, TY_FUNC, TY_ARRAY } TypeKind;

struct Type {
    TypeKind kind;
    int size;       // sizeof() value

    // Pointer or array
    Type *base;

    // Declaration
    Token *name;

    // Array
    int array_len;

    // Function type
    Type *return_ty;
    Type *params;
    Type *next;
};

extern Type *ty_int;

bool is_integer(Type *ty);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int size);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Program *prog);
