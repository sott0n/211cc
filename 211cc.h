#include <stdio.h>

// Enum values expresses stype of token.
enum {
    TK_NUM = 256,   // integer token
    TK_IDENT,       // Identifier
    TK_EOF,         // End of input token
};

// Define a type of AST.
enum {
    ND_NUM = 256,
    ND_INDENT,
};

// Type of token.
typedef struct {
    int ty;         // token type
    int val;        // if ty is TK_NUM, val is number.
    char *input;    // token string
} Token;

// Type of node.
typedef struct Node {
    int ty;             // Operator or ND_NUM
    struct Node *lhs;   // Left-hand side
    struct Node *rhs;   // Right-hand side
    int val;            // Use it if only ty is ND_NUM
    char name;          // Use it if only ty is ND_INDENT
} Node;

// Type of vector.
typedef struct {
    void **data;
    int capacity;
    int len;
} Vector;

extern Node *new_node(int ty, Node *lhs, Node *rhs);
extern Node *new_node_num(int val);
extern Node *new_node_ident(int val);
extern Node *mul();
extern Node *add();
extern Node *term();
extern Node *stmt();
extern Node *assign();

extern Vector *new_vector();

extern void error(char *fmt, ...);
extern int consume(int ty);
extern void vec_push(Vector *vec, void *elem);
extern void program();
extern void tokenize(char *p);
extern void vec_push();
extern void ge_lval(Node *node);
extern void gen(Node *node);
extern void runtest();

extern Token tokens[];
extern Node *code[];

extern int pos;
