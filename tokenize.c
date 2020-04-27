#include "211cc.h"

Token *token;

Token *new_token(TokenKind kind, Token *cur, char *str, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->str = str;
    tok->len = len;
    cur->next = tok;
    return tok;
}

bool startswith(char *p, char *q) {
    return memcmp(p, q, strlen(q)) == 0;
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

        if (startswith(p, "==") || startswith(p, "!=") ||
            startswith(p, "<=") || startswith(p, ">=")) {
                cur = new_token(TK_RESERVED, cur, p, 2);
                p += 2;
                continue;
            }

        if (strchr("+-*/()<>;=!", *p)) {
            cur = new_token(TK_RESERVED, cur, p++, 1);
            continue;
        }

        if (isdigit(*p)) {
            cur = new_token(TK_NUM, cur, p, 0);
            char *q = p;
            cur->val = strtol(p, &p, 10);
            cur->len = p - q;
            continue;
        }

        if ('a' <= *p && *p <= 'z') {
            cur = new_token(TK_IDENT, cur, p, 1);
            cur->name = *p;
            p++;
            continue;
        }

        error("Cannot tokenize: %s\n", p);
    }
    new_token(TK_EOF, cur, p, 0);
    return head.next;
}