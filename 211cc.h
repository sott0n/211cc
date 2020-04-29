#define _GNU_SOURCE
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

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
Token *tokenize(char *input);

//
// parse.c
//

// Local variable
typedef struct Var Var;
struct Var {
    Var *next;      // next Var
    char *name;     // Variable name
    int offset;     // Offset from RBP
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
    ND_RETURN,      // "return"
    ND_IF,          // "if"
    ND_FOR,         // "for" or "while"
    ND_BLOCK,       // { ... }
    ND_EXPR_STMT,   // Expression statement
    ND_VAR,         // Variable
    ND_NUM,         // integer
} NodeKind;

// Type of node
typedef struct Node Node;
struct Node {
    NodeKind kind;      // Node kind
    Node *next;         // Next node
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

    Var *var;           // Use it if only kind == ND_VAR
    long val;           // Use it if only kind == ND_NUM
};

typedef struct Function Function;
struct Function {
    Node *node;
    Var *locals;
    int stack_size;
};

Function *parse(Token *tok);

//
// codegen.c
//
void codegen(Function *prog);
