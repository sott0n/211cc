#include "211cc.h"

// Input filename
static char *current_filename;

// Input string
static char *current_input;

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(int lineno, char *loc, char *fmt, va_list ap) {
    // Find a line containing `loc`
    char *line = loc;
    while (current_input < line && line[-1] != '\n')
        line--;

    char *end = loc;
    while (*end != '\n')
        end++;

    // Print out the line
    int indent = fprintf(stderr, "%s:%d: ", current_filename, lineno);
    fprintf(stderr, "%.*s\n", (int)(end - line), line);

    // Show the error message
    int pos = loc - line + indent;

    fprintf(stderr, "%*s", pos, "");
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

static void error_at(char *loc, char *fmt, ...) {
    int lineno = 1;
    for (char *p = current_input; p < loc; p++)
        if (*p == '\n')
            lineno++;

    va_list ap;
    va_start(ap, fmt);
    verror_at(lineno, loc, fmt, ap);
    exit(1);
}

void error_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->lineno, tok->loc, fmt, ap);
    exit(1);
}

void warn_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->lineno, tok->loc, fmt, ap);
}

bool equal(Token *tok, char *op) {
    return strlen(op) == tok->len &&
            !strncmp(tok->loc, op, tok->len);
}

// Ensure that the current token is `op`
Token *skip(Token *tok, char *op) {
    if (!equal(tok, op))
        error_tok(tok, "expected '%s'", op);
    return tok->next;
}

bool consume(Token **rest, Token *tok, char *str) {
    if (equal(tok, str)) {
        *rest = tok->next;
        return true;
    }
    *rest = tok;
    return false;
}

// Create a new token and add it as the next token of `cur`
static Token *new_token(TokenKind kind, Token *cur, char *str, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->loc = str;
    tok->len = len;
    cur->next = tok;
    return tok;
}

static bool startswith(char *p, char *q) {
    return strncmp(p, q, strlen(q)) == 0;
}

static bool is_alpha(char c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static bool is_alnum(char c) {
    return is_alpha(c) || ('0' <= c && c <= '9');
}

static bool is_hex(char c) {
    return ('0' <= c && c <= '9') ||
           ('a' <= c && c <= 'f') ||
           ('A' <= c && c <= 'F');
}

static int from_hex(char c) {
    if ('0' <= c && c <= '9')
        return c - '0';
    if ('a' <= c && c <= 'f')
        return c - 'a' + 10;
    return c - 'A' + 10;
}

static bool is_keyword(Token *tok) {
    static char *kw[] = {
        "return", "if", "else", "for", "while", "int", "sizeof", "char",
        "struct", "union", "short", "long", "void", "typedef", "_Bool",
        "enum", "static",
    };

    for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
        if (equal(tok, kw[i]))
            return true;
    return false;
}

static void convert_keywords(Token *tok) {
    for (Token *t = tok; t->kind != TK_EOF; t = t->next)
        if (t->kind == TK_IDENT && is_keyword(t))
            t->kind = TK_RESERVED;
}

static char *read_escaped_char(char *result, char *p) {
    switch (*p) {
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7': {
        // Read an octal number
        int r = *p++ - '0';
        if ('0' <= *p && *p <= '7') {
            r = (r << 3) | (*p++ - '0');
            if ('0' <= *p && *p <= '7')
                r = (r << 3) | (*p++ - '0');
        }
        *result = r;
        return p;
    }
    case 'x': {
        // Read a hexadecimal number
        p++;
        if (!is_hex(*p))
            error_at(p, "invalid hex escape sequence");

        int r = 0;
        for (; is_hex(*p); p++) {
            r = (r << 4) | from_hex(*p);
            if (r > 255)
                error_at(p, "hex escape sequence out of range");
        }
        *result = r;
        return p;
    }
    case 'a': *result = '\a'; return p + 1;
    case 'b': *result = '\b'; return p + 1;
    case 't': *result = '\t'; return p + 1;
    case 'n': *result = '\n'; return p + 1;
    case 'v': *result = '\v'; return p + 1;
    case 'f': *result = '\f'; return p + 1;
    case 'r': *result = '\r'; return p + 1;
    case 'e': *result = 27; return p + 1;
    default: *result = *p; return p + 1;
    }
}

static Token *read_string_literal(Token *cur, char *start) {
    char *p = start + 1;
    char *end = p;

    // Find the closing double-quote
    for (; *end != '"'; end++) {
        if (*end == '\0')
            error_at(start, "unclosed string literal");
        if (*end == '\\')
            end++;
    }

    // Allocate a buffer that is large enough to hold the entire string
    char *buf = malloc(end - p + 1);
    int len = 0;

    while (*p != '"') {
        if (*p == '\\') {
            char c;
            p = read_escaped_char(&c, p + 1);
            buf[len++] = c;
        } else {
            buf[len++] = *p++;
        }
    }

    buf[len++] = '\0';

    Token *tok = new_token(TK_STR, cur, start, p - start + 1);
    tok->contents = buf;
    tok->cont_len = len;
    return tok;
}

static Token *read_char_literal(Token *cur, char *start) {
    char *p = start + 1;
    if (*p == '\0')
        error_at(start, "unclosed char literal");

    char c;
    if (*p == '\\')
        p = read_escaped_char(&c, p + 1);
    else
        c = *p++;

    if (*p != '\'')
        error_at(p, "char literal too long");
    p++;

    Token *tok = new_token(TK_NUM, cur, start, p - start);
    tok->val = c;
    return tok;
}

// Initialize line into for all tokens
static void add_line_info(Token *tok) {
    char *p = current_input;
    int lineno = 1;

    do {
        if (p == tok->loc) {
            tok->lineno;
            tok = tok->next;
        }
        if (*p == '\n')
            lineno++;
    } while (*p++);
}

// Tokenize a given string and returns new tokens
Token *tokenize(char *filename, char *p) {
    current_filename = filename;
    current_input = p;

    Token head = {};
    Token *cur = &head;

    while (*p) {
        // Skip newline character
        if (*p == '\n') {
            p++;
            continue;
        }

        // Skip whitespace
        if (isspace(*p)) {
            p++;
            continue;
        }

        // Skip line comments
        if (startswith(p, "//")) {
            p += 2;
            while (*p != '\n')
                p++;
            continue;
        }

        // Skip block comments
        if (startswith(p, "/*")) {
            char *q = strstr(p + 2, "*/");
            if (!q)
                error_at(p, "unclosed block comment");
            p = q + 2;
            continue;
        }

        // Single literal
        if (*p == '"') {
            cur = read_string_literal(cur, p);
            p += cur->len;
            continue;
        }

        // Character literal
        if (*p == '\'') {
            cur = read_char_literal(cur, p);
            p += cur->len;
            continue;
        }

        // Identifier
        if (is_alpha(*p)) {
            char *q = p++;
            while (is_alnum(*p))
                p++;
            cur = new_token(TK_IDENT, cur, q, p - q);
            continue;
        }

        // Multi-letter punctuators
        if (startswith(p, "==") || startswith(p, "!=") ||
            startswith(p, "<=") || startswith(p, ">=") ||
            startswith(p, "->")) {
            cur = new_token(TK_RESERVED, cur, p, 2);
            p += 2;
            continue;
        }

        // Single-letter puctuators
        if (ispunct(*p)) {
            cur = new_token(TK_RESERVED, cur, p++, 1);
            continue;
        }

        // Integer literal
        if (isdigit(*p)) {
            cur = new_token(TK_NUM, cur, p, 0);
            char *q = p;
            cur->val = strtol(p, &p, 10);
            cur->len = p - q;
            continue;
        }

        error_at(p, "invalid token");
    }

    new_token(TK_EOF, cur, p, 0);
    add_line_info(head.next);
    convert_keywords(head.next);
    return head.next;
}
