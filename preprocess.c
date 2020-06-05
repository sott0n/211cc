#include "211cc.h"

// Returns the contents of a given file
static char *read_file_string(char *path) {
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

// Entry point function of the preprocessor
Token *read_file(char *path) {
    char *input = read_file_string(path);
    Token *tok = tokenize(path, input);
    convert_keywords(tok);
    return tok;
}
