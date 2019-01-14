#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "211cc.h"

// Save tokenized value into this list
// not over number 100.
Token tokens[100];

// Save each code splitted ';'.
Node *code[100];

// Recursive-decendent parser
int pos = 0;

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Incorrect number of args\n");
        return 1;
    }

    char *arg = argv[1];

    if (strcmp(arg, "-test") == 0) {
        runtest();
        return 0;
    } else {
        // Using tokenize to parse.
        tokenize(arg);
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
}