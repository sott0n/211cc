#include "211cc.h"

static int top;
static int labelseq = 1;
static char *argreg8[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *argreg16[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static char *funcname;

static char *reg(int idx) {
    static char *r[] = {"r10", "r11", "r12", "r13", "r14", "r15"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static char *regx(Type *ty, int idx) {
    if (ty->base || size_of(ty) == 8)
        return reg(idx);

    static char *r[] = {"r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: $d", idx);
    return r[idx];
}

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

// pushes the given node's address to the stack
static void gen_addr(Node *node) {
    switch (node->kind) {
    case ND_VAR:
        if (node->var->is_local)
            printf("  lea %s, [rbp-%d]\n", reg(top++), node->var->offset);
        else
            printf("  mov %s, offset %s\n", reg(top++), node->var->name);
        return;
    case ND_DEREF:
        gen_expr(node->lhs);
        return;
    case ND_MEMBER:
        gen_addr(node->lhs);
        printf("  add %s, %d\n", reg(top - 1), node->member->offset);
        return;
    case ND_COMMA:
        gen_expr(node->lhs);
        top--;
        gen_addr(node->rhs);
        return;
    }

    error_tok(node->tok, "not an lvalue");
}

// Load a value from where the stack top is pointing to.
static void load(Type *ty) {
    if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT) {
        // If it is an array, do nothing because in general we can't load
        // an entire array to a register. As a result, the result of an
        // evaluation of an array becomes not the array itself but the
        // address of the array. In other words, this is where "array is
        // automatically converted to a pointer to the first element of
        // the array in C" occurs.
        return;
    }

    char *rs = reg(top - 1);
    char *rd = regx(ty, top -1);
    int sz = size_of(ty);

    // When we load a char or a short value to a register, we always
    // extend them to the size of int, so we can assume the lower half of
    // a register always contains a valid value. The upper half of a
    // register for char, short and int may contain garbage. When we load
    // a long value to a register, it simply occupies the entire register.
    if (sz == 1)
        printf("  movsx %s, byte ptr [%s]\n", rd, rs);
    else if (sz == 2)
        printf("  movsx %s, word ptr [%s]\n", rd, rs);
    else if (sz == 4)
        printf("  mov %s, dword ptr [%s]\n", rd, rs);
    else
        printf("  mov %s, [%s]\n", rd, rs);
}

static void store(Type *ty) {
    char *rd = reg(top - 1);
    char *rs = reg(top - 2);
    int sz = size_of(ty);

    if (ty->kind == TY_STRUCT) {
        for (int i = 0; i < sz; i++) {
            printf("  mov al, [%s+%d]\n", rs, i);
            printf("  mov [%s+%d], al\n", rd, i);
        }
    } else if (sz == 1) {
        printf("  mov [%s], %sb\n", rd, rs);
    } else if (sz == 2) {
        printf("  mov [%s], %sw\n", rd, rs);
    } else if (sz == 4) {
        printf("  mov [%s], %sd\n", rd, rs);
    } else {
        printf("  mov [%s], %s\n", rd, rs);
    }

    top--;
}

static void cast(Type *from, Type *to) {
    if (to->kind == TY_VOID)
        return;

    char *r = reg(top - 1);
    
    if (to->kind == TY_BOOL) {
        printf("  cmp %s, 0\n", r);
        printf("  setne %sb\n", r);
        printf("  movzx %s, %sb\n", r, r);
        return;
    }

    if (size_of(to) == 1)
        printf("  movsx %s, %sb\n", r, r);
    else if (size_of(to) == 2)
        printf("  movsx %s, %sw\n", r, r);
    else if (size_of(to) == 4)
        printf("  mov %sd, %sd\n", r, r);
    else if (!from->base && size_of(from) < 8)
        printf("  movsx %s, %sd\n", r, r);
}

static void divmod(Node *node, char *rd, char *rs, char *r64, char *r32) {
    if (size_of(node->ty) == 8) {
        printf("  mov rax, %s\n", rd);
        printf("  cqo\n");
        printf("  idiv %s\n", rs);
        printf("  mov %s, %s\n", rd, r64);
    } else {
        printf("  mov eax, %s\n", rd);
        printf("  cdq\n");
        printf("  idiv %s\n", rs);
        printf("  mov %s, %s\n", rd, r32);
    }
}

// generate code for a given node
static void gen_expr(Node *node) {
    printf(".loc 1 %d\n", node->tok->lineno);

    switch (node->kind) {
    case ND_NUM:
        printf("  mov %s, %ld\n", reg(top++), node->val);
        return;
    case ND_VAR:
    case ND_MEMBER:
        gen_addr(node);
        load(node->ty);
        return;
    case ND_DEREF:
        gen_expr(node->lhs);
        load(node->ty);
        return;
    case ND_ADDR:
        gen_addr(node->lhs);
        return;
    case ND_ASSIGN:
        if (node->ty->kind == TY_ARRAY)
            error_tok(node->tok, "not an lvalue");

        gen_expr(node->rhs);
        gen_addr(node->lhs);
        store(node->ty);
        return;
    case ND_STMT_EXPR:
        for (Node *n = node->body; n; n = n->next)
            gen_stmt(n);
        top++;
        return;
    case ND_CAST:
        gen_expr(node->lhs);
        cast(node->lhs->ty, node->ty);
        return;
    case ND_COMMA:
        gen_expr(node->lhs);
        top--;
        gen_expr(node->rhs);
        return;
    case ND_NOT:
        gen_expr(node->lhs);
        printf("  cmp %s, 0\n", reg(top - 1));
        printf("  sete %sb\n", reg(top - 1));
        printf("  movzx %s, %sb\n", reg(top - 1), reg(top - 1));
        return;
    case ND_BITNOT:
        gen_expr(node->lhs);
        printf("  not %s\n", reg(top - 1));
        return;
    case ND_FUNCALL: {
        // Save all temporary registers to the stack before evaluating
        // function arguments to allow each argument evaluation to use all
        // temporary registers. This is a workaround for a register
        // exhaustion issue when evaluating a long expression containing
        // multiple function calls.
        int top_orig = top;
        top = 0;

        printf("  push r10\n");
        printf("  push r11\n");
        printf("  push r12\n");
        printf("  push r13\n");
        printf("  push r14\n");
        printf("  push r15\n");

        int nargs = 0;
        for (Node *arg = node->args; arg; arg = arg->next) {
            gen_expr(arg);
            printf("  push %s\n", reg(--top));
            printf("  sub rsp, 8\n");
            nargs++;
        }

        for (int i = nargs - 1; i >= 0; i--) {
            printf("  add rsp, 8\n");
            printf("  pop %s\n", argreg64[i]);
        }

        printf("  mov rax, 0\n");
        printf("  call %s\n", node->funcname);

        top = top_orig;
        printf("  pop r15\n");
        printf("  pop r14\n");
        printf("  pop r13\n");
        printf("  pop r12\n");
        printf("  pop r11\n");
        printf("  pop r10\n");

        printf("  mov %s, rax\n", reg(top++));
        return;
    }
    }

    // Binary expressions
    gen_expr(node->lhs);
    gen_expr(node->rhs);

    char *rd = regx(node->lhs->ty, top - 2);
    char *rs = regx(node->lhs->ty, top - 1);
    top--;

    switch (node->kind) {
    case ND_ADD:
        printf("  add %s, %s\n", rd, rs);
        return;
    case ND_SUB:
        printf("  sub %s, %s\n", rd, rs);
        return;
    case ND_MUL:
        printf("  imul %s, %s\n", rd, rs);
        return;
    case ND_DIV:
        divmod(node, rd, rs, "rax", "eax");
        return;
    case ND_MOD:
        divmod(node, rd, rs, "rdx", "edx");
        return;
    case ND_BITAND:
        printf("  and %s, %s\n", rd, rs);
        return;
    case ND_BITOR:
        printf("  or %s, %s\n", rd, rs);
        return;
    case ND_BITXOR:
        printf("  xor %s, %s\n", rd, rs);
        return;
    case ND_EQ:
        printf("  cmp %s, %s\n", rd, rs);
        printf("  sete al\n");
        printf("  movzx %s, al\n", rd);
        return;
    case ND_NE:
        printf("  cmp %s, %s\n", rd, rs);
        printf("  setne al\n");
        printf("  movzx %s, al\n", rd);
        return;
    case ND_LT:
        printf("  cmp %s, %s\n", rd, rs);
        printf("  setl al\n");
        printf("  movzx %s, al\n", rd);
        return;
    case ND_LE:
        printf("  cmp %s, %s\n", rd, rs);
        printf("  setle al\n");
        printf("  movzx %s, al\n", rd);
        return;
    default:
        error_tok(node->tok, "invalid expression");
    }
}

static void gen_stmt(Node *node) {
    printf(".loc 1 %d\n", node->tok->lineno);

    switch (node->kind) {
    case ND_IF: {
        int seq = labelseq++;
        if (node->els) {
            gen_expr(node->cond);
            printf("  cmp %s, 0\n", reg(--top));
            printf("  je  .L.else.%d\n", seq);
            gen_stmt(node->then);
            printf("  jmp .L.end.%d\n", seq);
            printf(".L.else.%d:\n", seq);
            gen_stmt(node->els);
            printf(".L.end.%d:\n", seq);
        } else {
            gen_expr(node->cond);
            printf("  cmp %s, 0\n", reg(--top));
            printf("  je  .L.end.%d\n", seq);
            gen_stmt(node->then);
            printf(".L.end.%d:\n", seq);
        }
        return;
    }
    case ND_FOR: {
        int seq = labelseq++;
        if (node->init)
            gen_stmt(node->init);
        printf(".L.begin.%d:\n", seq);
        if (node->cond) {
            gen_expr(node->cond);
            printf("  cmp %s, 0\n", reg(--top));
            printf("  je .L.end.%d\n", seq);
        }
        gen_stmt(node->then);
        if (node->inc)
            gen_stmt(node->inc);
        printf("  jmp .L.begin.%d\n", seq);
        printf(".L.end.%d:\n", seq);
        return;
    }
    case ND_BLOCK:
        for (Node *n = node->body; n; n = n->next)
            gen_stmt(n);
        return;
    case ND_RETURN:
        gen_expr(node->lhs);
        printf("  mov rax, %s\n", reg(--top));
        printf("  jmp .L.return.%s\n", funcname);
        return;
    case ND_EXPR_STMT:
        gen_expr(node->lhs);
        top--;
        return;
    default:
        error_tok(node->tok, "invalid statement");
    }
}

static char *get_argreg(int sz, int idx) {
    if (sz == 1)
        return argreg8[idx];
    if (sz == 2)
        return argreg16[idx];
    if (sz == 4)
        return argreg32[idx];
    assert(sz == 8);
    return argreg64[idx];
}

static void emit_data(Program *prog) {
    printf(".data\n");

    for (Var *var = prog->globals; var; var = var->next) {
        printf("%s:\n", var->name);

        if (!var->contents) {
            printf("  .zero %d\n", size_of(var->ty));
            continue;
        }

        for (int i = 0; i < var->cont_len; i++)
            printf("  .byte %d\n", var->contents[i]);
    }
}

static void emit_text(Program *prog) {
    printf(".text\n");

    for (Function *fn = prog->fns; fn; fn = fn->next) {
        if (!fn->is_static)
            printf(".globl %s\n", fn->name);
        printf("%s:\n", fn->name);
        funcname = fn->name;

        // Prologue. r12-15 are callee-saved registers
        printf("  push rbp\n");
        printf("  mov rbp, rsp\n");
        printf("  sub rsp, %d\n", fn->stack_size);
        printf("  mov [rbp-8], r12\n");
        printf("  mov [rbp-16], r13\n");
        printf("  mov [rbp-24], r14\n");
        printf("  mov [rbp-32], r15\n");

        // Save arguments to the stack
        int i = 0;
        for (Var *var = fn->params; var; var = var->next)
            i++;

        for (Var *var = fn->params; var; var = var->next) {
            char *r = get_argreg(size_of(var->ty), --i);
            printf("  mov [rbp-%d], %s\n", var->offset, r);
        }

        // Emit code
        for (Node *n = fn->node; n; n = n->next) {
            gen_stmt(n);
            assert(top == 0);
        }

        // Epilogue
        printf(".L.return.%s:\n", funcname);
        printf("  mov r12, [rbp-8]\n");
        printf("  mov r13, [rbp-16]\n");
        printf("  mov r14, [rbp-24]\n");
        printf("  mov r15, [rbp-32]\n");
        printf("  mov rsp, rbp\n");
        printf("  pop rbp\n");
        printf("  ret\n");
    }
}


void codegen(Program *prog) {
    printf(".intel_syntax noprefix\n");
    emit_data(prog);
    emit_text(prog);
}
