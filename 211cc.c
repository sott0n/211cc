#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Enum values expresses stype of token.
enum {
    TK_NUM = 256,   // integer token
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

void tokenize(char *p) {
    int i = 0;
    while (*p) {
        // Skip whitespace.
        if (isspace(*p)) {
            p++;
            continue;
        }

        if (*p == '+' || *p == '-') {
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

        fprintf(stderr, "Cannot tokenize: %s\n", p);
        exit(1);
    }

    tokens[i].ty = TK_EOF;
    tokens[i].input = p;
}

void error(int i) {
    fprintf(stderr, "Unexpected token: %s\n",
            tokens[i].input);
    exit(1);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Incorrect number of args.\n");
        return 1;
    }

    tokenize(argv[1]);

    printf(".intel_syntax noprefix\n");
    printf(".global main\n");
    printf("main:\n");

    if (tokens[0].ty != TK_NUM)
        error(0);
    printf("  mov rax, %d\n", tokens[0].val);

    int i = 1;
    while (tokens[i].ty != TK_EOF) {
        if (tokens[i].ty == '+') {
            i++;
            if (tokens[i].ty != TK_NUM)
                error(i);
            printf("  add rax, %d\n", tokens[i].val);
            i++;
            continue;
        }

        if (tokens[i].ty == '-') {
            i++;
            if (tokens[i].ty != TK_NUM)
                error(i);
            printf("  sub rax, %d\n", tokens[i].val);
            i++;
            continue;
        }

        error(i);
    }

    printf("  ret\n");
    return 0;
}