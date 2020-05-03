#include "211cc.h"

static int align_to(int n, int align) {
    return (n + align - 1) & ~(align - 1);
}

// Returns the contents of a given file
static char *read_file(char *path) {
    // By convention, read from stdin in a given filename is "-"
    FILE *fp = stdin;
    if (strcmp(path, "-")) {
        fp = fopen(path, "r");
        if (!fp)
            error("cannot open %s: %s", path, strerror(errno));
    }

    int buflen = 4096;
    int nread = 0;
    char *buf = malloc(buflen);

    // Read the entire file
    for (;;) {
        int end = buflen - 2; // extra 2bytes for the tailing "\n\0"
        int n = fread(buf + nread, 1, end - nread, fp);
        if (n == 0)
            break;
        nread += n;
        if (nread == end) {
            buflen *= 2;
            buf = realloc(buf, buflen);
        }
    }

    if (fp != stdin)
        fclose(fp);

    // Canonicalize the last line by appending "\n"
    // if it does not end with a newline.
    if (nread == 0 || buf[nread - 1] != '\n')
        buf[nread++] = '\n';
    buf[nread] = '\0';
    return buf;
}

int main(int argc, char **argv) {
    if (argc != 2)
        error("%s: invalid number of arguments", argv[0]);

    // Using tokenize to parse.
    char *input = read_file(argv[1]);
    Token *tok = tokenize(argv[1], input);
    Program *prog = parse(tok);

    // Assign offsets to local variables
    for (Function *fn = prog->fns; fn; fn = fn->next) {
        int offset = 32; // 32 for callee-saved registers
        for (Var *var = fn->locals; var; var = var->next) {
            offset += var->ty->size;
            var->offset = offset;
        }
        fn->stack_size = align_to(offset, 16);
    }

    // Traverse the AST to emit assembly
    codegen(prog);

    return 0;
}