#include <ctype.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include "211cc.h"

// Save each code splitted ';'.
Node *code[100];

Token *token;

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void error_at(char *loc, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    int pos = loc - user_input;
    fprintf(stderr, "%s\n", user_input);
    fprintf(stderr, "%*s", pos, "");
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

Node *new_node(TokenKind kind, Node *lhs, Node *rhs) {
    Node *node = malloc(sizeof(Node));
    node->kind = kind;
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *new_node_num(int val) {
    Node *node = malloc(sizeof(Node));
    node->kind = ND_NUM;
    node->val = val;
    return node;
}

Node *new_node_ident(char name) {
    Node *node = malloc(sizeof(Node));
    node->kind = ND_INDENT;
    node->name = name;
    token = token->next;
    return node;
}

bool consume(char op) {
    if (token->str[0] != op)
        return false;
    token = token->next;
    return true;
}

void expect(char op) {
    if (token->str[0] != op)
        error_at(token->str, "expected '%c'", op);
    token = token->next;
}

int expect_number() {
    if (token->kind != TK_NUM)
        error_at(token->str, "expected a number");
    int val = token->val;
    token = token->next;
    return val;
}

bool at_eof() {
    return token->kind == TK_EOF;
}

Token *new_token(TokenKind kind, Token *cur, char *str) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->str = str;
    cur->next = tok;
    return tok;
}

Node *stmt() {
  Node *node = assign();
}

Node *assign() {
    Node *node = add();

    for (;;) {
        if (consume('='))
            return new_node(ND_ASSIGN, node, assign());

        if (consume(';'))
            return node;

        if (token->kind == TK_EQUAL) {
            return new_node(ND_EQUAL, node, assign());
        }

        if (token->kind == TK_NEQUAL) {
            return new_node(ND_NEQUAL, node, assign());
        }

        else
            return node;
    }
}

Node *add() {
    Node *node = mul();

    for (;;) {
        if(consume('+'))
            node = new_node(ND_ADD, node, mul());
        else if (consume('-'))
            node = new_node(ND_SUB, node, mul());
        else
            return node;
    }
}

Node *mul() {
    Node *node = unary();

    for (;;) {
        if (consume('*'))
            node = new_node(ND_MUL, node, unary());
        else if (consume('/'))
            node = new_node(ND_DIV, node, unary());
        else
            return node;
    }
}

Node *unary() {
    if (consume('+'))
        return unary();
    if (consume('-'))
        return new_node(ND_SUB, new_node_num(0), unary());
    return term();
}

Node *term() {
    if (consume('(')) {
        Node *node = add();
        if (!consume(')'))
            error_at(token->str, "Not have a closing parenthesis corresponds to the bracket: %s",
                    token->str);
        return node;
    }

    if (token->kind == TK_NUM)
        return new_node_num(expect_number());

    if (token->kind == TK_IDENT)
        return new_node_ident(token->name);

    error_at(token->str, "Not expected token: %s", token->str);
}

void program() {
    int i = 0;
    while (!at_eof()) {
        code[i++] = stmt();
    }
    code[i] = NULL;
}

Token *tokenize() {
    char *p = user_input;
    Token head;
    head.next = NULL;
    Token *cur = &head;

    while (*p) {
        // Skip newline character.
        if (*p == '\n') {
            p++;
            continue;
        }

        // Skip whitespace.
        if (isspace(*p)) {
            p++;
            continue;
        }

        if (*p == '+' || *p == '-' || *p == '*' || *p == '/' ||
             *p == '(' || *p == ')' || *p == ';' || *p == '=' ||
             *p == '!') {

            if ((*p == '=' || *p == '!') && *(p+1) == '=') {
                if (*p == '=') {
                    cur = new_token(TK_EQUAL, cur, p);
                } else {
                    cur = new_token(TK_NEQUAL, cur, p);
                }
                p += 2;
                continue;
            }

            cur = new_token(TK_RESERVED, cur, p++);
            continue;
        }

        if (isdigit(*p)) {
            cur = new_token(TK_NUM, cur, p);
            cur->val = strtol(p, &p, 10);
            continue;
        }

        if ('a' <= *p && *p <= 'z') {
            cur = new_token(TK_IDENT, cur, p);
            cur->name = *p;
            p++;
            continue;
        }

        error("Cannot tokenize: %s\n", p);
    }
    new_token(TK_EOF, cur, p);
    return head.next;
}