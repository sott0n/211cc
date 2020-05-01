#include "211cc.h"

static int top;
static int labelseq = 1;
static char *argreg[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static char *funcname;

static char *reg(int idx) {
    static char *r[] = {"r10", "r11", "r12", "r13", "r14", "r15"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static void gen_expr(Node *node);

// pushes the given node's address to the stack
static void gen_addr(Node *node) {
    switch (node->kind) {
    case ND_VAR:
        printf("  lea %s, [rbp-%d]\n", reg(top++), node->var->offset);
        return;
    case ND_DEREF:
        gen_expr(node->lhs);
        return;
    }

    error_tok(node->tok, "not an lvalue");
}

// Load a value from where the stack top is pointing to.
static void load(Type *ty) {
    if (ty->kind == TY_ARRAY) {
        // If it is an array, do nothing because in general we can't load
        // an entire array to a register. As a result, the result of an
        // evaluation of an array becomes not the array itself but the
        // address of the array. In other words, this is where "array is
        // automatically converted to a pointer to the first element of
        // the array in C" occurs.
        return;
    }

    printf("  mov %s, [%s]\n", reg(top - 1), reg(top - 1));
}

static void store(void) {
    printf("  mov [%s], %s\n", reg(top - 1), reg(top - 2));
    top--;
}

// generate code for a given node
static void gen_expr(Node *node) {
    switch (node->kind) {
    case ND_NUM:
        printf("  mov %s, %ld\n", reg(top++), node->val);
        return;
    case ND_VAR:
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
        store();
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
            printf("  pop %s\n", argreg[i]);
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

    gen_expr(node->lhs);
    gen_expr(node->rhs);

    char *rd = reg(top - 2);
    char *rs = reg(top - 1);
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
        printf("  mov rax, %s\n", rd);
        printf("  cqo\n");
        printf("  idiv %s\n", rs);
        printf("  mov %s, rax\n", rd);
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

void codegen(Function *prog) {
    printf(".intel_syntax noprefix\n");
    for (Function *fn = prog; fn; fn = fn->next) {
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
        for (Var *var = fn->params; var; var = var->next)
            printf("  mov [rbp-%d], %s\n", var->offset, argreg[--i]);

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