#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "211cc.h"

// Save tokenized value into this list
// not over number 100.
Token tokens[100];

// Save each code splitted ';'.
Node *code[100];

// Recursive-decendent parser
int pos = 0;

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

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

int consume(int ty) {
    if (tokens[pos].ty != ty)
        return 0;
    pos++;
    return 1;
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

    if (tokens[pos].ty == TK_IDENT)
        return new_node_ident(tokens[pos++].val);

    error("Not expected token: %s", tokens[pos].input);
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

Node *stmt() {
  Node *node = assign();
//   if (!consume(';'))
//     error("Expect token is';', but got: %s", tokens[pos].input);
}

Node *assign() {
    Node *node = add();

    for (;;) {
        if (consume('='))
            return new_node('=', node, assign());

        if (consume(';'))
            return node;

        if (tokens[pos].ty == TK_EQUAL) {
            pos++;
            return new_node(ND_EQUAL, node, assign());
        }

        if (tokens[pos].ty == TK_NEQUAL) {
            pos++;
            return new_node(ND_NEQUAL, node, assign());
        }

        else
            return node;
    }
}

void program() {
  int i = 0;
  while (tokens[pos].ty != TK_EOF)
    code[i++] = stmt();
  code[i] = NULL;
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
             *p == '(' || *p == ')' || *p == ';' || *p == '=' ||
             *p == '!') {

            if ((*p == '=' || *p == '!') && *(p+1) == '=') {
                if (*p == '=') {
                    tokens[i].ty = ND_EQUAL;
                } else {
                    tokens[i].ty = ND_NEQUAL;
                }
                tokens[i].input = p;
                i++;
                p += 2;
                continue;
            }

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