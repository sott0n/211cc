#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Enum values expresses stype of token.
enum {
    TK_NUM = 256,   // integer token
    TK_IDENT,       // Identifier
    TK_EOF,         // End of input token
};

// Type of token.
typedef struct {
    int ty;         // token type
    int val;        // if ty is TK_NUM, val is number.
    char *input;    // token string
} Token;

// Save tokenized value into this list
// not over number 100.
Token tokens[100];

// Recursive-decendent parser
int pos = 0;

// Define a type of AST.
enum {
    ND_NUM = 256,
    ND_INDENT,
};

typedef struct Node {
    int ty;             // Operator or ND_NUM
    struct Node *lhs;   // Left-hand side
    struct Node *rhs;   // Right-hand side
    int val;            // Use it if only ty is ND_NUM
    char name;          // Use it if only ty is ND_INDENT
} Node;

Node *new_node(int ty, Node *lhs, Node *rhs) {
    Node *node = malloc(sizeof(Node));
    node->ty = ty;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_node_num(int val) {
    Node *node = malloc(sizeof(Node));
    node->ty = ND_NUM;
    node->val = val;
    return node;
}

Node *new_node_ident(int val) {
    Node *node = malloc(sizeof(Node));
    node->ty = ND_INDENT;
    node->val = val;
    return node;
}

int consume(int ty) {
    if (tokens[pos].ty != ty)
        return 0;
    pos++;
    return 1;
}

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

Node *term();
Node *assign();
Node *mul();
Node *add();

Node *mul() {
    Node *node = term();

    for (;;) {
        if (consume('*'))
            node = new_node('*', node, term());
        else if (consume('/'))
            node = new_node('/', node, term());
        else
            return node;
    }
}

Node *add() {
    Node *node = mul();

    for (;;) {
        if(consume('+'))
            node = new_node('+', node, mul());
        else if (consume('-'))
            node = new_node('-', node, mul());
        else
            return node;
    }
}

Node *term() {
    if (consume('(')) {
        Node *node = add();
        if (!consume(')'))
            error("Not have a closing parenthesis corresponds to the bracket: %s",
                    tokens[pos].input);
        return node;
    }

    if (tokens[pos].ty == TK_NUM)
        return new_node_num(tokens[pos++].val);
    else if (tokens[pos].ty == TK_IDENT)
        return new_node_ident(tokens[pos++].val);

    error("It not expected token: %s", tokens[pos].input);
}

Node *code[100];

Node *program() {
    int i = 0;
    while (tokens[pos].ty != TK_EOF) {
        code[i++] = assign();
    }
    code[i] = NULL;
}

Node *assign() {
    Node *node = add();

    for (;;) {
        if (consume('='))
            node = new_node('=', node, assign());
        if (consume(';'))
            return node;
        else
            return node;
    }
}

void tokenize(char *p) {
    int i = 0;
    while (*p) {
        // Skip whitespace.
        if (isspace(*p)) {
            p++;
            continue;
        }

        if (*p == '+' || *p == '-' || *p == '*' || *p == '/' ||
             *p == '(' || *p == ')' || *p == '=' || *p == ';') {
            tokens[i].ty = *p;
            tokens[i].input = p;
            i++;
            p++;
            continue;
        }

        if (isdigit(*p)) {
            tokens[i].ty = TK_NUM;
            tokens[i].input = p;
            tokens[i].val = strtol(p, &p, 10);
            i++;
            continue;
        }

        if ('a' <= *p && *p <= 'z') {
            tokens[i].ty = TK_IDENT;
            tokens[i].input = p;
            i++;
            p++;
            continue;
        }

        fprintf(stderr, "Cannot tokenize: %s\n", p);
        exit(1);
    }

    tokens[i].ty = TK_EOF;
    tokens[i].input = p;
}

void gen_lval(Node *node) {
    if (node->ty != ND_INDENT)
        error("Left value is not identifier");

    int offset = ('z' - node->name + 1) * 8;
    printf("  mov rax, rbp\n");
    printf("  sub rax, %d\n", offset);
    printf("  push rax\n");
}

void gen(Node *node) {
    if (node->ty == ND_NUM) {
        printf("  push %d\n", node->val);
        return;
    }

    if (node->ty == ND_INDENT) {
        gen_lval(node);
        printf("  pop rax\n");
        printf("  mov rax, [rax]\n");
        printf("  push rax\n");
        return;
    }

    if (node->ty == '=') {
        gen_lval(node->lhs);
        gen(node->rhs);

        printf("  pop rdi\n");
        printf("  pop rax\n");
        printf("  mov [rax], rdi\n");
        printf("  push rdi\n");
        return;
    }

    gen(node->lhs);
    gen(node->rhs);

    printf("  pop rdi\n");
    printf("  pop rax\n");

    switch (node->ty) {
    case '+':
        printf("  add rax, rdi\n");
        break;
    case '-':
        printf("  sub rax, rdi\n");
        break;
    case '*':
        printf("  mul rdi\n");
        break;
    case '/':
        printf("  mov rdx, 0\n");
        printf("  div rdi\n");
    }

    printf("  push rax\n");
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Incorrect number of args\n");
        return 1;
    }

    // Using tokenize to parse.
    tokenize(argv[1]);
    program();

    // Header of asemble.
    printf(".intel_syntax noprefix\n");
    printf(".global main\n");
    printf("main:\n");

    // Prologue
    // Get a range for 26 variables.
    printf("  push rbp\n");
    printf("  mov rbp, rsp\n");
    printf("  sub rsp, 208\n");

    // Generate code.
    for (int i = 0; code[i]; i++) {
        gen(code[i]);
        printf("  pop rax\n");
    }

    // Epilogue
    // Sinse the result of the last expression remains in rax,
    // it becomes a return value.
    printf("  mov rsp, rbp\n");
    printf("  pop rbp\n");
    printf("  ret\n");
    return 0;
}