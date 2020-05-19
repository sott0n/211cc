#define _GNU_SOURCE
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

typedef struct Type Type;
typedef struct Member Member;

//
// tokenize.c
//

// Token
typedef enum {
    TK_RESERVED,    // Keywords or puctuators
    TK_NUM,         // Integer literals
    TK_IDENT,       // Identifiers
    TK_STR,         // String literals
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

    char *contents; // String literal contents including terminating '\0'
    char cont_len;  // String literal length

    int lineno;     // Line number
};

void error(char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
void warn_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize(char *filename, char *input);

//
// parse.c
//

// variable
typedef struct Var Var;
struct Var {
    Var *next;      // next Var
    char *name;     // Variable name
    Type *ty;       // Type
    bool is_local;  // local or global

    // Local variable
    int offset;

    // Global variable
    char *contents;
    int cont_len;
};

// Define a type of AST
typedef enum {
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_MOD,         // %
    ND_BITAND,      // &
    ND_BITOR,       // |
    ND_BITXOR,      // ^
    ND_LOGAND,      // &&
    ND_LOGOR,       // ||
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <
    ND_LE,          // <=
    ND_ASSIGN,      // =
    ND_COMMA,       // ,
    ND_MEMBER,      // . (struct member access)
    ND_ADDR,        // unary &
    ND_DEREF,       // unary *
    ND_NOT,         // !
    ND_BITNOT,      // ~
    ND_RETURN,      // "return"
    ND_IF,          // "if"
    ND_FOR,         // "for" or "while"
    ND_BLOCK,       // { ... }
    ND_FUNCALL,     // Function call
    ND_EXPR_STMT,   // Expression statement
    ND_STMT_EXPR,   // Statement expression
    ND_VAR,         // Variable
    ND_NUM,         // integer
    ND_CAST,        // Type cast
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

    // Block or statement expression
    Node *body;

    // Struct member access
    Member *member;

    // Function call
    char *funcname;
    Type *func_ty;
    Node *args;

    Var *var;           // Use it if only kind == ND_VAR
    long val;           // Use it if only kind == ND_NUM
};

typedef struct Function Function;
struct Function {
    Function *next;
    char *name;
    Var *params;
    bool is_static;

    Node *node;
    Var *locals;
    int stack_size;
};

typedef struct {
    Var *globals;
    Function *fns;
} Program;

Node *new_cast(Node *expr, Type *ty);
Program *parse(Token *tok);

//
// typing.c
//

typedef enum {
    TY_VOID,
    TY_BOOL,
    TY_CHAR,
    TY_SHORT,
    TY_INT,
    TY_LONG,
    TY_ENUM,
    TY_PTR,
    TY_FUNC,
    TY_ARRAY,
    TY_STRUCT,
} TypeKind;

struct Type {
    TypeKind kind;
    int size;           // sizeof() value
    int align;          // alignment
    bool is_incomplete; // incomplete type

    // Pointer or array
    Type *base;

    // Declaration
    Token *name;

    // Array
    int array_len;

    // Struct
    Member *members;

    // Function type
    Type *return_ty;
    Type *params;
    Type *next;
};

// Struct member
struct Member {
    Member *next;
    Type *ty;
    Token *tok; // for error message
    Token *name;
    int offset;
};

extern Type *ty_void;
extern Type *ty_bool;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

bool is_integer(Type *ty);
Type *copy_type(Type *ty);
int align_to(int n, int align);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int size);
Type *enum_type(void);
int size_of(Type *ty);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Program *prog);
