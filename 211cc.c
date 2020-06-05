#include "211cc.h"

static void usage(void) {
    fprintf(stderr, "211cc [ -I<path> ] <file>\n");
    exit(1);
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

    // Emit a .file direc"tive for the assembler.
    printf(".file 1 \"%s\"\n", path);
    return buf;
}

int main(int argc, char **argv) {
    char *filename = NULL;

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--help"))
            usage();

        if (argv[i][0] == '-' && argv[i][1] != '\0')
            error("unknown argument: %s", argv[i]);

        filename = argv[i];
    }

    if (!filename)
        error("no input file");

    // Using tokenize to parse.
    char *input = read_file(filename);
    Token *tok = tokenize(filename, input);
    Program *prog = parse(tok);

    // Assign offsets to local variables
    for (Function *fn = prog->fns; fn; fn = fn->next) {
        // Besides local variables, callee-saved registers take 32 bytes
        // and the variable-argument save area takes 56 bytes in the stack.
        int offset = fn->is_varargs ? 88 : 32;

        for (Var *var = fn->locals; var; var = var->next) {
            offset = align_to(offset, var->align);
            offset += size_of(var->ty);
            var->offset = offset;
        }
        fn->stack_size = align_to(offset, 16);
    }

    // Traverse the AST to emit assembly
    codegen(prog);

    return 0;
}
