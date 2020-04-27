#include <stdio.h>
#include <stdbool.h>

// Enum values expresses stype of token.
typedef enum {
    TK_RESERVED,    // Symbol
    TK_NUM,         // integer token
    TK_IDENT,       // Identifier
    TK_EOF,         // End of input token
} TokenKind;

typedef struct Token Token;

// Type of token.
struct Token {
    TokenKind kind; // token type
    Token *next;    // next token
    int val;        // if ty is TK_NUM, val is number.
    int name;       // token name
    char *str;      // token string
    int len;        // token length
};

// Define a type of AST.
typedef enum {
    ND_NUM = 256,   // integer
    ND_LVAR,        // indent
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <
    ND_LE,          // <=
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_ASSIGN,      // =
} AstKind;

// Type of node.
typedef struct Node {
    TokenKind kind;     // Operator or ND_NUM
    struct Node *lhs;   // Left-hand side
    struct Node *rhs;   // Right-hand side
    int val;            // Use it if only ty is ND_NUM
    int offset;         // Use it if only ty is ND_LVAL
} Node;

// Type of vector.
typedef struct {
    void **data;
    int capacity;
    int len;
} Vector;

extern Node *new_node(TokenKind kind, Node *lhs, Node *rhs);
extern Node *new_node_num(int val);
extern Node *new_node_ident();
extern Node *expr();
extern Node *stmt();
extern Node *assign();
extern Node *equality();
extern Node *relational();
extern Node *add();
extern Node *mul();
extern Node *unary();
extern Node *term();

extern Vector *new_vector();

extern void error(char *fmt, ...);
extern void error_at(char *loc, char *fmt, ...);
extern bool consume(char *op);
extern void expect(char *op);
extern int expect_number();
extern bool at_eof();
extern Token *new_token(TokenKind kind, Token *cur, char *str, int len);
extern bool startswith(char *p, char *q);
extern void vec_push(Vector *vec, void *elem);
extern void program();
extern Token *tokenize();
extern void vec_push();
extern void ge_lval(Node *node);
extern void gen(Node *node);
extern void runtest();

extern Node *code[];
extern Token *token;
extern char *user_input;
