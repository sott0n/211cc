// This file contains a recursive descent parser for C.
//
// Most functions in this file are named after the symbols they are
// supposed to read from an input token list. For example, stmt() is
// responsible for reading a statement from a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens. Since C doesn't support
// multiple return values, the remaining tokens are returned to the
// caller via a pointer argument.
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this
// parser.

#include "211cc.h"

// Scope for local, global variables or typedefs.
// or enum constants
typedef struct VarScope VarScope;
struct VarScope {
    VarScope *next;
    char *name;
    int depth;

    Var *var;
    Type *type_def;
    Type *enum_ty;
    int enum_val;
};

// Scope for struct, union or enum tags
typedef struct TagScope TagScope;
struct TagScope {
    TagScope *next;
    char *name;
    int depth;
    Type *ty;
};

// Variable attributes such as typedef or extern.
typedef struct {
    bool is_typedef;
    bool is_static;
    bool is_extern;
    int align;
} VarAttr;

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
typedef struct Initializer Initializer;
struct Initializer {
    Type *ty;
    Token *tok;

    // If len is 0, it' a leaf node, and `expr` has an initializer
    // expression. Otherwise, `children` has child nodes.
    int len;
    Node *expr;

    // `childlen` may contain null pointers if elements are omitted.
    // For example, an initializer for `int x[100] = {1}` has 99 null
    // pointers at the end of `children`.
    //
    // The C spec requires that, if an initializer is given, members
    // with no initializers will automatically be initialized with
    // zeros. So null childrens are equivalent to zeros.
    Initializer **children;
};

// All local variable instances created during parsing are
// accumulated to this list.
static Var *locals;

// Likewise, global variables are accumulated to this list.
static Var *globals;

// C has two block scopes: one is for variables/typedefs and
// the other is for struct/union/enum tags.
static VarScope *var_scope;
static TagScope *tag_scope;

// scope_depth is incremented by one at "{" and decremented
// by one at "}".
static int scope_depth;

// Points to the function object the parser is currently parsing.
static Var *current_fn;

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
static Node *current_switch;

static bool is_typename(Token *tok);
static Type *typespec(Token **rest, Token *tok, VarAttr *attr);
static Type *typename(Token **rest, Token *tok);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *type_suffix(Token **rest, Token *tok, Type *ty);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok);
static Initializer *initializer(Token **rest, Token *tok, Type *ty);
static Node *lvar_initializer(Token **rest, Token *tok, Var *var);
static GvarInitializer *gvar_initializer(Token **rest, Token *tok, Type *ty);
static GvarInitializer *gvar_init_string(char *p, int len);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static long eval(Node *node);
static long eval2(Node *node, Var **var);
static Node *assign(Token **rest, Token *tok);
static Node *logor(Token **rest, Token *tok);
static long const_expr(Token **rest, Token *tok);
static Node *conditional(Token **rest, Token *tok);
static Node *logand(Token **rest, Token *tok);
static Node *bitor(Token **rest, Token *tok);
static Node *bitxor(Token **rest, Token *tok);
static Node *bitand(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *shift(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *new_add(Node *lhs, Node *rhs, Token *tok);
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

static void enter_scope(void) {
    scope_depth++;
}

static void leave_scope(void) {
    scope_depth--;

    while (var_scope && var_scope->depth > scope_depth)
        var_scope = var_scope->next;

    while (tag_scope && tag_scope->depth > scope_depth)
        tag_scope = tag_scope->next;
}

// Find a variable or a typedef by name.
static VarScope *find_var(Token *tok) {
    for (VarScope *sc = var_scope; sc; sc = sc->next)
        if (strlen(sc->name) == tok->len && !strncmp(tok->loc, sc->name, tok->len))
            return sc;
    return NULL;
}

static TagScope *find_tag(Token *tok) {
    for (TagScope *sc = tag_scope; sc; sc = sc->next)
        if (strlen(sc->name) == tok->len && !strncmp(tok->loc, sc->name, tok->len))
            return sc;
    return NULL;
}

static Node *new_node(NodeKind kind, Token *tok) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->tok = tok;
    return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
    Node *node = new_node(kind, tok);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
    Node *node = new_node(kind, tok);
    node->lhs = expr;
    return node;
}

static Node *new_num(long val, Token *tok) {
    Node *node = new_node(ND_NUM, tok);
    node->val = val;
    return node;
}

static Node *new_ulong(long val, Token *tok) {
    Node *node = new_node(ND_NUM, tok);
    node->val = val;
    node->ty = ty_ulong;
    return node;
}

static Node *new_var_node(Var *var, Token *tok) {
    Node *node = new_node(ND_VAR, tok);
    node->var = var;
    return node;
}

Node *new_cast(Node *expr, Type *ty) {
    add_type(expr);

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_CAST;
    node->tok = expr->tok;
    node->lhs = expr;
    node->ty = copy_type(ty);
    return node;
}

static VarScope *push_scope(char *name) {
    VarScope *sc = calloc(1, sizeof(VarScope));
    sc->next = var_scope;
    sc->name = name;
    sc->depth = scope_depth;
    var_scope = sc;
    return sc;
}

static Initializer *new_init(Type *ty, int len, Node *expr, Token *tok) {
    Initializer *init = calloc(1, sizeof(Initializer));
    init->ty = ty;
    init->tok = tok;
    init->len = len;
    init->expr = expr;
    if (len)
        init->children = calloc(len, sizeof(Initializer *));
    return init;
}

static Var *new_lvar(char *name, Type *ty) {
    Var *var = calloc(1, sizeof(Var));
    var->name = name;
    var->ty = ty;
    var->align = ty->align;
    var->is_local = true;
    var->next = locals;
    locals = var;
    push_scope(name)->var = var;
    return var;
}

static Var *new_gvar(char *name, Type *ty, bool is_static, bool emit) {
    Var *var = calloc(1, sizeof(Var));
    var->name = name;
    var->ty = ty;
    var->align = ty->align;
    var->is_local = false;
    var->is_static = is_static;
    if (emit) {
        var->next = globals;
        globals = var;
    }
    push_scope(name)->var = var;
    return var;
}

static char *new_label(void) {
    static int cnt = 0;
    char *buf = malloc(20);
    sprintf(buf, ".L.data.%d", cnt++);
    return buf;
}

static Var *new_string_literal(char *p, int len) {
    Type *ty = array_of(ty_char, len);
    Var *var = new_gvar(new_label(), ty, true, true);
    var->initializer = gvar_init_string(p, len);
    return var;
}

static bool is_end(Token *tok) {
    return equal(tok, "}") || (equal(tok, ",") && equal(tok->next, "}"));
}

static Token *expect_end(Token *tok) {
    if (equal(tok, "}"))
        return tok->next;
    if (equal(tok, ",") && equal(tok->next, "}"))
        return tok->next->next;
    error_tok(tok, "expected '}'");
}

static char *get_ident(Token *tok) {
    if (tok->kind != TK_IDENT)
        error_tok(tok, "expected a identifier");
    return strndup(tok->loc, tok->len);
}

static Type *find_typedef(Token *tok) {
    if (tok->kind == TK_IDENT) {
        VarScope *sc = find_var(tok);
        if (sc)
            return sc->type_def;
    }
    return NULL;
}

static long get_number(Token *tok) {
    if (tok->kind != TK_NUM)
        error_tok(tok, "expected a number");
    return tok->val;
}

static void push_tag_scope(Token *tok, Type *ty) {
    TagScope *sc = calloc(1, sizeof(TagScope));
    sc->next = tag_scope;
    sc->name = strndup(tok->loc, tok->len);
    sc->depth = scope_depth;
    sc->ty = ty;
    tag_scope = sc;
}

// funcdef = typename declarator compound-stmt
static Function *funcdef(Token **rest, Token *tok) {
    locals = NULL;

    VarAttr attr = {};
    Type *ty = typespec(&tok, tok, &attr);
    ty = declarator(&tok, tok, ty);

    Function *fn = calloc(1, sizeof(Function));
    fn->name = get_ident(ty->name);
    fn->is_static = attr.is_static;
    fn->is_varargs = ty->is_varargs;

    enter_scope();
    for (Type *t = ty->params; t; t = t->next)
        new_lvar(get_ident(t->name), t);
    fn->params = locals;

    tok = skip(tok, "{");
    fn->node = compound_stmt(rest, tok)->body;
    fn->locals = locals;
    leave_scope();
    return fn;
}

// typespec = typename typename *
// typename = "void" | "_Bool" | "char" | "short" | "int" | "long"
//            | struct-decl | union-decl | typedef-name
//
// The order of typenames in type-specifier doesn't matter. For
// example, `int long static` means the same as `static long int`.
// That can also be written as `static long` because you can omit
// `int` if `long` or `short` are specified. However, something like
// `char int` is not a valid type specifier. We have to accept only a
// limited combinations of the typenames.
//
// In this function, we count the number of occurrences of each typename
// while keeping the "current" type object that the typenames up
// until that point represent. When we reach a non-typename token,
// we returns the current type object.
static Type *typespec(Token **rest, Token *tok, VarAttr *attr) {
    // We use a single integer as counters for all typenames.
    // For example, bits 0 and 1 represents how many times we saw the
    // keyword "void" so far. With this, we can use a switch statement
    // as you can see below.
    enum {
        VOID     = 1 << 0,
        BOOL     = 1 << 2,
        CHAR     = 1 << 4,
        SHORT    = 1 << 6,
        INT      = 1 << 8,
        LONG     = 1 << 10,
        OTHER    = 1 << 12,
        SIGNED   = 1 << 13,
        UNSIGNED = 1 << 14,
    };

    Type *ty = ty_int;
    int counter = 0;
    bool is_const = false;

    while (is_typename(tok)) {
        // Handle storage class specifiers
        if (equal(tok, "typedef") || equal(tok, "static") || equal(tok, "extern")) {
            if (!attr)
                error_tok(tok, "storage class specifier is not allowed in this context");

            if (equal(tok, "typedef"))
                attr->is_typedef = true;
            else if (equal(tok, "static"))
                attr->is_static = true;
            else
                attr->is_extern = true;

            if (attr->is_typedef + attr->is_static + attr->is_extern > 1)
                error_tok(tok, "typedef and static may not be used together");
            tok = tok->next;
            continue;
        }

        if (consume(&tok, tok, "const")) {
            is_const = true;
            continue;
        }

        if (equal(tok, "_Alignas")) {
            if (!attr)
                error_tok(tok, "_Alignas is not allowed in this context");
            tok = skip(tok->next, "(");

            if (is_typename(tok))
                attr->align = typename(&tok, tok)->align;
            else
                attr->align = const_expr(&tok, tok);
            tok = skip(tok, ")");
            continue;
        }

        // Handle user-defined types.
        Type *ty2 = find_typedef(tok);
        if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") || ty2) {
            if (counter)
                break;

            if (equal(tok, "struct")) {
                ty = struct_decl(&tok, tok->next);
            } else if (equal(tok, "union")) {
                ty = union_decl(&tok, tok->next);
            } else if (equal(tok, "enum")) {
                ty = enum_specifier(&tok, tok->next);
            } else {
                ty = ty2;
                tok = tok->next;
            }

            counter += OTHER;
            continue;
        }

        // Handle built-in types.
        if (equal(tok, "void"))
            counter += VOID;
        else if (equal(tok, "_Bool"))
            counter += BOOL;
        else if (equal(tok, "char"))
            counter += CHAR;
        else if (equal(tok, "short"))
            counter += SHORT;
        else if (equal(tok, "int"))
            counter += INT;
        else if (equal(tok, "long"))
            counter += LONG;
        else if (equal(tok, "signed"))
            counter |= SIGNED;
        else if (equal(tok, "unsigned"))
            counter |= UNSIGNED;
        else
            error_tok(tok, "internal error");

        switch (counter) {
        case VOID:
            ty = ty_void;
            break;
        case BOOL:
            ty = ty_bool;
            break;
        case CHAR:
        case SIGNED + CHAR:
            ty = ty_char;
            break;
        case UNSIGNED + CHAR:
            ty = ty_uchar;
            break;
        case SHORT:
        case SHORT + INT:
        case SIGNED + SHORT:
        case SIGNED + SHORT + INT:
            ty = ty_short;
            break;
        case UNSIGNED + SHORT:
        case UNSIGNED + SHORT + INT:
            ty = ty_ushort;
            break;
        case INT:
        case SIGNED:
        case SIGNED + INT:
            ty = ty_int;
            break;
        case UNSIGNED:
        case UNSIGNED + INT:
            ty = ty_uint;
            break;
        case LONG:
        case LONG + INT:
        case LONG + LONG:
        case LONG + LONG + INT:
        case SIGNED + LONG:
        case SIGNED + LONG + INT:
        case SIGNED + LONG + LONG:
        case SIGNED + LONG + LONG + INT:
            ty = ty_long;
            break;
        case UNSIGNED + LONG:
        case UNSIGNED + LONG + INT:
        case UNSIGNED + LONG + LONG:
        case UNSIGNED + LONG + LONG + INT:
            ty = ty_ulong;
            break;
        default:
            error_tok(tok, "invalid type");
        }

        tok = tok->next;
    }

    if (is_const) {
        ty = copy_type(ty);
        ty->is_const = true;
    }

    *rest = tok;
    return ty;
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = typespec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
    if (equal(tok, "void") && equal(tok->next, ")")) {
        *rest = tok->next->next;
        return func_type(ty);
    }

    Type head = {};
    Type *cur = &head;
    bool is_varargs = false;

    while (!equal(tok, ")")) {
        if (cur != &head)
            tok = skip(tok, ",");

        if (equal(tok, "...")) {
            is_varargs = true;
            tok = tok->next;
            skip(tok, ")");
            break;
        }

        Type *ty2 = typespec(&tok, tok, NULL);
        ty2 = declarator(&tok, tok, ty2);

        // "array of T" is converted to "pointer to T" only in the parameter
        // context. For example, *argv[] is converted to **argv by this.
        if (ty2->kind == TY_ARRAY) {
            Token *name = ty2->name;
            ty2 = pointer_to(ty2->base);
            ty2->name = name;
        }

        cur = cur->next = copy_type(ty2);
    }

    ty = func_type(ty);
    ty->params = head.next;
    ty->is_varargs = is_varargs;
    *rest = tok->next;
    return ty;
}

// array-dimensions = const-expr? "]" type-suffix
static Type *array_dimensions(Token **rest, Token *tok, Type *ty) {
    if (equal(tok, "]")) {
        ty = type_suffix(rest, tok->next, ty);
        ty = array_of(ty, 0);
        ty->is_incomplete = true;
        return ty;
    }

    int sz = const_expr(&tok, tok);
    tok = skip(tok, "]");
    ty = type_suffix(rest, tok, ty);
    return array_of(ty, sz);
}

// type-suffix = "(" func-params
//             | "[" array-dimensions
//             | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
    if (equal(tok, "("))
        return func_params(rest, tok->next, ty);

    if (equal(tok, "[")) {
        return array_dimensions(rest, tok->next, ty);
    }

    *rest = tok;
    return ty;
}

// pointers = ("*" "const"*)*
static Type *pointers(Token **rest, Token *tok, Type *ty) {
    while (consume(&tok, tok, "*")) {
        ty = pointer_to(ty);
        while (consume(&tok, tok, "const"))
            ty->is_const = true;
    }
    *rest = tok;
    return ty;
}

// abstract-declarator = pointers ("(" abstract-declarator "))? type-suffix
static Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
    ty = pointers(&tok, tok, ty);

    if (equal(tok, "(")) {
        Type *placeholder = calloc(1, sizeof(Type));
        Type *new_ty = abstract_declarator(&tok, tok->next, placeholder);
        tok = skip(tok, ")");
        *placeholder = *type_suffix(rest, tok, ty);
        return new_ty;
    }

    return type_suffix(rest, tok, ty);
}

// type-name = typespec abstract_declarator
static Type *typename(Token **rest, Token *tok) {
    Type *ty = typespec(&tok, tok, NULL);
    return abstract_declarator(rest, tok, ty);
}

// Some types of list can end with an optional "," followed by "}"
// to allow a trailing comma, This function returns true if it looks
// like we are at the end of such list.
static bool consume_end(Token **rest, Token *tok) {
    if (equal(tok, "}")) {
        *rest = tok->next;
        return true;
    }

    if (equal(tok, ",") && equal(tok->next, "}")) {
        *rest = tok->next->next;
        return true;
    }
    
    *rest = tok;
    return false;
}

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
//
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
static Type *enum_specifier(Token **rest, Token *tok) {
    Type *ty = enum_type();

    // Read a struct tag
    Token *tag = NULL;
    if (tok->kind == TK_IDENT) {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !equal(tok, "{")) {
        TagScope *sc = find_tag(tag);
        if (!sc)
            error_tok(tag, "unknown enum type");
        if (sc->ty->kind != TY_ENUM)
            error_tok(tag, "not an enum type");
        *rest = tok;
        return sc->ty;
    }

    tok = skip(tok, "{");

    // Read an enum-list
    int i = 0;
    int val = 0;
    while (!is_end(tok)) {
        if (i++ > 0)
            tok = skip(tok, ",");

        char *name = get_ident(tok);
        tok = tok->next;

        if (equal(tok, "="))
            val = const_expr(&tok, tok->next);

        VarScope *sc = push_scope(name);
        sc->enum_ty = ty;
        sc->enum_val = val++;
    }

    if (tag)
        push_tag_scope(tag, ty);
    *rest = expect_end(tok);
    return ty;
}

// declarator = pointers ("(" declarator ")" | ident) type-suffix
static Type *declarator(Token **rest, Token *tok, Type *ty) {
    ty = pointers(&tok, tok, ty);

    if (equal(tok, "(")) {
        Type *placeholder = calloc(1, sizeof(Type));
        Type *new_ty = declarator(&tok, tok->next, placeholder);
        tok = skip(tok, ")");
        *placeholder = *type_suffix(rest, tok, ty);
        return new_ty;
    }

    if (tok->kind != TK_IDENT)
        error_tok(tok, "expected a variable name");

    ty = type_suffix(rest, tok->next, ty);
    ty->name = tok;
    return ty;
}

// declaration = typespec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok) {
    VarAttr attr = {};
    Type *basety = typespec(&tok, tok, &attr);

    Node head = {};
    Node *cur = &head;
    int cnt = 0;

    while (!equal(tok, ";")) {
        if (cnt++ > 0)
            tok = skip(tok, ",");

        Type *ty = declarator(&tok, tok, basety);
        if (ty->kind == TY_VOID)
            error_tok(tok, "variable declared void");

        if (attr.is_typedef) {
            push_scope(get_ident(ty->name))->type_def = ty;
            continue;
        }

        if (attr.is_static) {
            // static local variable
            Var *var = new_gvar(new_label(), ty, true, true);
            push_scope(get_ident(ty->name))->var = var;

            if (equal(tok, "="))
                var->initializer = gvar_initializer(&tok, tok->next, ty);
            continue;
        }

        Var *var = new_lvar(get_ident(ty->name), ty);
        if (attr.align)
            var->align = attr.align;

        if (equal(tok, "="))
            cur = cur->next = lvar_initializer(&tok, tok->next, var);
    }

    Node *node = new_node(ND_BLOCK, tok);
    node->body = head.next;
    *rest = tok->next;
    return node;
}

static void skip_excess_elements(Token **rest, Token *tok) {
    while (!consume_end(&tok, tok)) {
        tok = skip(tok, ",");
        if (equal(tok, "{"))
            skip_excess_elements(&tok, tok->next);
        else
            assign(&tok, tok);
    }
    *rest = tok;
}

static Token *skip_end(Token *tok) {
    if (consume_end(&tok, tok))
        return tok;
    warn_tok(tok, "excess elements in initializer");
    skip_excess_elements(&tok, tok);
    return tok;
}

// string-initializer = string-literal
static Initializer *string_initializer(Token **rest, Token *tok, Type *ty) {
    // Initialize a char array with a string literal
    if (ty->is_incomplete) {
        ty->size = tok->cont_len;
        ty->array_len = tok->cont_len;
        ty->is_incomplete = false;
    }

    Initializer *init = new_init(ty, ty->array_len, NULL, tok);

    int len = (ty->array_len < tok->cont_len)
        ? ty->array_len : tok->cont_len;

    for (int i = 0; i < len; i++) {
        Node *expr = new_num(tok->contents[i], tok);
        init->children[i] = new_init(ty->base, 0, expr, tok);
    }
    *rest = tok->next;
    return init;
}

// array-initializer = "{" initializer ("," initializer)* ","? "}"
//                   | initializer ("," initializer)* ","
static Initializer *array_initializer(Token **rest, Token *tok, Type *ty) {
    bool has_paren = consume(&tok, tok, "{");

    if (ty->is_incomplete) {
        int i = 0;
        for (Token *tok2 = tok; !is_end(tok2); i++) {
            if (i > 0)
                tok2 = skip(tok2, ",");
            initializer(&tok2, tok2, ty->base);
        }

        ty->size = size_of(ty->base) * i;
        ty->array_len = i;
        ty->is_incomplete = false;
    }

    Initializer *init = new_init(ty, ty->array_len, NULL, tok);

    for (int i = 0; i < ty->array_len && !is_end(tok); i++) {
        if (i > 0)
            tok = skip(tok, ",");
        init->children[i] = initializer(&tok, tok, ty->base);
    }

    if (has_paren)
        tok = skip_end(tok);
    *rest = tok;
    return init;
}

// struct-initializer = "{" initializer ("," initializer)* ","? "}"
//                    | initializer ("," initializer)* ","
static Initializer *struct_initializer(Token **rest, Token *tok, Type *ty) {
    if (!equal(tok, "{")) {
        Token *tok2;
        Node *expr = assign(&tok2, tok);
        add_type(expr);
        if (expr->ty->kind == TY_STRUCT) {
            Initializer *init = new_init(ty, 0, expr, tok);
            *rest = tok2;
            return init;
        }
    }

    int len = 0;
    for (Member *mem = ty->members; mem; mem = mem->next)
        len++;

    Initializer *init = new_init(ty, len, NULL, tok);
    bool has_paren = consume(&tok, tok, "{");

    int i = 0;
    for (Member *mem = ty->members; mem && !is_end(tok); mem = mem->next, i++) {
        if (i > 0)
            tok = skip(tok, ",");
        init->children[i] = initializer(&tok, tok, mem->ty);
    }

    if (has_paren)
        tok = skip_end(tok);
    *rest = tok;
    return init;
}

// initializer = string-initializer | array-initializer | struct-initializer
//             | "{" assign "}"
//             | assign
static Initializer *initializer(Token **rest, Token *tok, Type *ty) {
    if (ty->kind == TY_ARRAY && ty->base->kind == TY_CHAR && tok->kind == TK_STR)
        return string_initializer(rest, tok, ty);

    if (ty->kind == TY_ARRAY)
        return array_initializer(rest, tok, ty);

    if (ty->kind == TY_STRUCT)
        return struct_initializer(rest, tok, ty);

    Token *start = tok;
    bool has_paren = consume(&tok, tok, "{");
    Initializer *init = new_init(ty, 0, assign(&tok, tok), start);
    if (has_paren)
        tok = skip_end(tok);
    *rest = tok;
    return init;
}

static Node *create_lvar_init(Node *cur, Initializer *init, Var *var, Type *ty, int offset) {
    if (ty->kind == TY_ARRAY) {
        int sz = size_of(ty->base);
        for (int i = 0; i < ty->array_len; i++) {
            Initializer *child = init ? init->children[i] : NULL;
            cur = create_lvar_init(cur, child, var, ty->base, offset + sz * i);
        }
        return cur;
    }

    if (ty->kind == TY_STRUCT && (!init || init->len)) {
        int i = 0;
        for (Member *mem = ty->members; mem; mem = mem->next, i++) {
            Initializer *child = init ? init->children[i] : NULL;
            cur = create_lvar_init(cur, child, var, mem->ty, offset + mem->offset);
        }
        return cur;
    }

    // Construct a node representing `*(&var+offset) = expr`.
    Token *tok = init ? init->tok : var->ty->name;
    Node *expr = init ? init->expr : new_num(0, tok);
    Node *ref = new_unary(ND_ADDR, new_var_node(var, tok), tok);
    Node *off = new_num(offset, tok);

    Node *expr2 =
        new_binary(ND_ASSIGN,
                   new_unary(ND_DEREF,
                             new_cast(new_binary(ND_ADD, ref, off, tok),
                                      pointer_to(ty)),
                             tok),
                   expr, tok);
    expr2->is_init = true;
    add_type(expr2);
    cur->next = new_unary(ND_EXPR_STMT, expr2, tok);
    return cur->next;
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[3] = {6, 7, 8}` is converted to the following expressions:
//
//   *(&x ⊕ 0) = 6;
//   *(&x ⊕ 4) = 7;
//   *(&x ⊕ 8) = 8;
//
// where `⊕` denotes not the pointer addition but the usual arithmetic
// addition (i.e. &x ⊕ y adds y instead of y scaled by sizeof(x)).
static Node *lvar_initializer(Token **rest, Token *tok, Var *var) {
    Initializer *init = initializer(rest, tok, var->ty);
    Node head = {};
    create_lvar_init(&head, init, var, var->ty, 0);

    Node *node = new_node(ND_BLOCK, tok);
    node->body = head.next;
    return node;
}

static GvarInitializer *
new_gvar_init_val(GvarInitializer *cur, int offset, int sz, int val) {
    GvarInitializer *init = calloc(1, sizeof(GvarInitializer));
    init->sz = sz;
    init->val = val;
    init->offset = offset;
    cur->next = init;
    return init;
}

static GvarInitializer *
new_gvar_init_label(GvarInitializer *cur, int offset, char *label, long addend) {
    GvarInitializer *init = calloc(1, sizeof(GvarInitializer));
    init->sz = 8;
    init->label = label;
    init->addend = addend;
    init->offset = offset;
    cur->next = init;
    return init;
}

static GvarInitializer *
create_gvar_init(GvarInitializer *cur, Initializer *init, Type *ty, int offset) {
    if (ty->kind == TY_ARRAY) {
        int sz = size_of(ty->base);
        for (int i = 0; i < ty->array_len; i++)
            if (init->children[i])
                cur = create_gvar_init(cur, init->children[i], ty->base, offset + sz * i);
        return cur;
    }

    if (ty->kind == TY_STRUCT) {
        int i = 0;
        for (Member *mem = ty->members; mem; mem = mem->next, i++)
            if (init->children[i])
                cur = create_gvar_init(cur, init->children[i], mem->ty, offset + mem->offset);
        return cur;
    }

    Var *var = NULL;
    long addend = eval2(init->expr, &var);
    if (var)
        return new_gvar_init_label(cur, offset, var->name, addend);
    return new_gvar_init_val(cur, offset, size_of(ty), addend);
}

// Initializers for global variables are evaluated at compile-time
// and embedded to .data section. This function converts Initializer
// objects to GvarInitializer objects. It is a compile error if an
// initializer list contains a non-constant expression.
static GvarInitializer *gvar_initializer(Token **rest, Token *tok, Type *ty) {
    Initializer *init = initializer(rest, tok, ty);
    GvarInitializer head = {};
    create_gvar_init(&head, init, ty, 0);
    return head.next;
}

// Construct GvarInitializer objects for a given string literal
static GvarInitializer *gvar_init_string(char *p, int len) {
    GvarInitializer head = {};
    GvarInitializer *cur = &head;
    for (int i = 0; i < len; i++)
        cur = new_gvar_init_val(cur, i, 1, p[i]);
    return head.next;
}

// Returns true if a given token represents a type
static bool is_typename(Token *tok) {
    static char *kw[] = {
        "void", "_Bool", "char", "short", "int", "long", "struct", "union",
        "typedef", "enum", "static", "extern", "_Alignas", "signed", "unsigned",
        "const",
    };

    for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
        if (equal(tok, kw[i]))
            return true;
    return find_typedef(tok);
}

// stmt = "return" expr? ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ":" stmt
//      | "default" ":" stmt
//      | "for" "(" (expr? ";" | declaration) expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "do" stmt "while" "(" expr ")" ";"
//      | "break" ";"
//      | "continue" ";"
//      | "goto" ident ";"
//      | ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr ";"
static Node *stmt(Token **rest, Token *tok) {
    if (equal(tok, "return")) {
        Node *node = new_node(ND_RETURN, tok);
        if (consume(rest, tok->next, ";"))
            return node;

        Node *exp = expr(&tok, tok->next);
        *rest = skip(tok, ";");

        add_type(exp);
        node->lhs = new_cast(exp, current_fn->ty->return_ty);
        return node;
    }

    if (equal(tok, "if")) {
        Node *node = new_node(ND_IF, tok);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");
        node->then = stmt(&tok, tok);
        if (equal(tok, "else"))
            node->els = stmt(&tok, tok->next);
        *rest = tok;
        return node;
    }

    if (equal(tok, "switch")) {
        Node *node = new_node(ND_SWITCH, tok);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");

        Node *sw = current_switch;
        current_switch = node;
        node->then = stmt(rest, tok);
        current_switch = sw;
        return node;
    }

    if (equal(tok, "case")) {
        if (!current_switch)
            error_tok(tok, "stray case");

        Node *node = new_node(ND_CASE, tok);
        int val = const_expr(&tok, tok->next);
        tok = skip(tok, ":");
        node->lhs = stmt(rest, tok);
        node->val = val;
        node->case_next = current_switch->case_next;
        current_switch->case_next = node;
        return node;
    }

    if (equal(tok, "default")) {
        if (!current_switch)
            error_tok(tok, "stray default");
        
        Node *node = new_node(ND_CASE, tok);
        tok = skip(tok->next, ":");
        node->lhs = stmt(rest, tok);
        current_switch->default_case = node;
        return node;
    }

    if (equal(tok, "for")) {
        Node *node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");

        enter_scope();
        if (is_typename(tok)) {
            node->init = declaration(&tok, tok);
        } else {
            if (!equal(tok, ";"))
                node->init = expr_stmt(&tok, tok);
            tok = skip(tok, ";");
        }

        if (!equal(tok, ";"))
            node->cond = expr(&tok, tok);
        tok = skip(tok, ";");

        if (!equal(tok, ")"))
            node->inc = expr_stmt(&tok, tok);
        tok = skip(tok, ")");

        node->then = stmt(rest, tok);
        leave_scope();
        return node;
    }

    if (equal(tok, "while")) {
        Node *node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");
        node->then = stmt(rest, tok);
        return node;
    }

    if (equal(tok, "do")) {
        Node *node = new_node(ND_DO, tok);
        node->then = stmt(&tok, tok->next);
        tok = skip(tok, "while");
        tok = skip(tok, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");
        *rest = skip(tok, ";");
        return node;
    }

    if (equal(tok, "break")) {
        *rest = skip(tok->next, ";");
        return new_node(ND_BREAK, tok);
    }

    if (equal(tok, "continue")) {
        *rest = skip(tok->next, ";");
        return new_node(ND_CONTINUE, tok);
    }

    if (equal(tok, "goto")) {
        Node *node = new_node(ND_GOTO, tok);
        node->label_name = get_ident(tok->next);
        *rest = skip(tok->next->next, ";");
        return node;
    }

    if (equal(tok, ";")) {
        Node *node = new_node(ND_BLOCK, tok);
        *rest = tok->next;
        return node;
    }

    if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
        Node *node = new_node(ND_LABEL, tok);
        node->label_name = strndup(tok->loc, tok->len);
        node->lhs = stmt(rest, tok->next->next);
        return node;
    }

    if (equal(tok, "{"))
        return compound_stmt(rest, tok->next);

    Node *node = expr_stmt(&tok, tok);
    *rest = skip(tok, ";");
    return node;
}

// compound-stmt = (declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
    Node *node = new_node(ND_BLOCK, tok);
    Node head = {};
    Node *cur = &head;

    enter_scope();

    while (!equal(tok, "}")) {
        if (is_typename(tok))
            cur = cur->next = declaration(&tok, tok);
        else
            cur = cur->next = stmt(&tok, tok);
        add_type(cur);
    }

    leave_scope();

    node->body = head.next;
    *rest = tok->next;
    return node;
}

// expr-stmt = expr
static Node *expr_stmt(Token **rest, Token *tok) {
    Node *node = new_node(ND_EXPR_STMT, tok);
    node->lhs= expr(rest, tok);
    return node;
}

// expr = assign ("," expr)??
static Node *expr(Token **rest, Token *tok) {
    Node *node = assign(&tok, tok);

    if (equal(tok, ","))
        return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);

    *rest = tok;
    return node;
}

static long eval(Node *node) {
    return eval2(node, NULL);
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a positive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
static long eval2(Node *node, Var **var) {
    add_type(node);

    switch (node->kind) {
    case ND_ADD:
        return eval2(node->lhs, var) + eval(node->rhs);
    case ND_SUB:
        return eval2(node->lhs, var) - eval(node->rhs);
    case ND_MUL:
        return eval(node->lhs) * eval(node->rhs);
    case ND_DIV:
        if (node->ty->is_unsigned)
            return (unsigned long)eval(node->lhs) / eval(node->rhs);
        return eval(node->lhs) / eval(node->rhs);
    case ND_BITAND:
        return eval(node->lhs) & eval(node->rhs);
    case ND_BITOR:
        return eval(node->lhs) | eval(node->rhs);
    case ND_BITXOR:
        return eval(node->lhs) ^ eval(node->rhs);
    case ND_SHL:
        return eval(node->lhs) << eval(node->rhs);
    case ND_SHR:
        if (node->ty->is_unsigned && size_of(node->ty) == 8)
            return (unsigned long)eval(node->lhs) >> eval(node->rhs);
        return eval(node->lhs) >> eval(node->rhs);
    case ND_EQ:
        return eval(node->lhs) == eval(node->rhs);
    case ND_NE:
        return eval(node->lhs) != eval(node->rhs);
    case ND_LT:
        if (node->ty->is_unsigned)
            return (unsigned long)eval(node->lhs) < eval(node->rhs);
        return eval(node->lhs) < eval(node->rhs);
    case ND_LE:
        if (node->ty->is_unsigned)
            return (unsigned long)eval(node->lhs) <= eval(node->rhs);
        return eval(node->lhs) <= eval(node->rhs);
    case ND_COND:
        return eval(node->cond) ? eval(node->then) : eval(node->els);
    case ND_COMMA:
        return eval(node->rhs);
    case ND_NOT:
        return !eval(node->lhs);
    case ND_BITNOT:
        return ~eval(node->lhs);
    case ND_LOGAND:
        return eval(node->lhs) && eval(node->rhs);
    case ND_LOGOR:
        return eval(node->lhs) || eval(node->rhs);
    case ND_CAST: {
        long val = eval2(node->lhs, var);
        if (!is_integer(node->ty) || size_of(node->ty) == 8)
            return val;

        switch (size_of(node->ty)) {
        case 1:
            if (node->ty->is_unsigned)
                return (unsigned char)val;
            return (char)val;
        case 2:
            if (node->ty->is_unsigned)
                return (unsigned short)val;
            return (short)val;
        default:
            assert(size_of(node->ty) == 4);
            if (node->ty->is_unsigned)
                return (unsigned int)val;
            return (int)val;
        }
    }
    case ND_NUM:
        return node->val;
    case ND_ADDR:
        if (!var || *var || node->lhs->kind != ND_VAR || node->lhs->var->is_local)
            error_tok(node->tok, "invalid initializer");
        *var = node->lhs->var;
        return 0;
    case ND_VAR:
        if (!var || *var || node->var->ty->kind != TY_ARRAY)
            error_tok(node->tok, "invalid initializer");
        *var = node->var;
        return 0;
    }

    error_tok(node->tok, "not a constant expression");
}

static long const_expr(Token **rest, Token *tok) {
    Node *node = conditional(rest, tok);
    return eval(node);
}

// Convert `A op=B` to `tmp = &A, *tmp = *tmp op B`
// where tmp is a fresh pointer variable.
static Node *to_assign(Node *binary) {
    add_type(binary->lhs);
    add_type(binary->rhs);

    Var *var = new_lvar("", pointer_to(binary->lhs->ty));
    Token *tok = binary->tok;

    Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
                             new_unary(ND_ADDR, binary->lhs, tok), tok);

    Node *expr2 =
        new_binary(ND_ASSIGN,
                   new_unary(ND_DEREF, new_var_node(var, tok), tok),
                   new_binary(binary->kind,
                              new_unary(ND_DEREF, new_var_node(var, tok), tok),
                              binary->rhs,
                              tok),
                   tok);

    return new_binary(ND_COMMA, expr1, expr2, tok);
}

// assign = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
static Node *assign(Token **rest, Token *tok) {
    Node *node = conditional(&tok, tok);

    if (equal(tok, "="))
        return new_binary(ND_ASSIGN, node, assign(rest, tok->next), tok);

    if (equal(tok, "+="))
        return to_assign(new_add(node, assign(rest, tok->next), tok));

    if (equal(tok, "-="))
        return to_assign(new_sub(node, assign(rest, tok->next), tok));
    
    if (equal(tok, "*="))
        return to_assign(new_binary(ND_MUL, node, assign(rest, tok->next), tok));

    if (equal(tok, "/="))
        return to_assign(new_binary(ND_DIV, node, assign(rest, tok->next), tok));

    if (equal(tok, "%="))
        return to_assign(new_binary(ND_MOD, node, assign(rest, tok->next), tok));

    if (equal(tok, "&="))
        return to_assign(new_binary(ND_BITAND, node, assign(rest, tok->next), tok));

    if (equal(tok, "|="))
        return to_assign(new_binary(ND_BITOR, node, assign(rest, tok->next), tok));

    if (equal(tok, "^="))
        return to_assign(new_binary(ND_BITXOR, node, assign(rest, tok->next), tok));

    if (equal(tok, "<<="))
        return to_assign(new_binary(ND_SHL, node, assign(rest, tok->next), tok));

    if (equal(tok, ">>="))
        return to_assign(new_binary(ND_SHR, node, assign(rest, tok->next), tok));

    *rest = tok;
    return node;
}

// conditional = logor ("?" expr ":" conditional)?
static Node *conditional(Token **rest, Token *tok) {
    Node *node = logor(&tok, tok);

    if (!equal(tok, "?")) {
        *rest = tok;
        return node;
    }

    Node *cond = new_node(ND_COND, tok);
    cond->cond = node;
    cond->then = expr(&tok, tok->next);
    tok = skip(tok, ":");
    cond->els = conditional(rest, tok);
    return cond;
}

// logor = logand ("||" logand)*
static Node *logor(Token **rest, Token *tok) {
    Node *node = logand(&tok, tok);
    while (equal(tok, "||")) {
        node = new_binary(ND_LOGOR, node, NULL, tok);
        node->rhs = logand(&tok, tok->next);
    }
    *rest = tok;
    return node;
}

// logand = bitor ("&&" bitor)*
static Node *logand(Token **rest, Token *tok) {
    Node *node = bitor(&tok, tok);
    while (equal(tok, "&&")) {
        node = new_binary(ND_LOGAND, node, NULL, tok);
        node->rhs = bitor(&tok, tok->next);
    }
    *rest = tok;
    return node;
}

// bitor = bitxor ("|" bitxor)*
static Node *bitor(Token **rest, Token *tok) {
    Node *node = bitxor(&tok, tok);
    while (equal(tok, "|")) {
        node = new_binary(ND_BITOR, node, NULL, tok);
        node->rhs = bitxor(&tok, tok->next);
    }
    *rest = tok;
    return node;
}

// bitxor = bitand ("^" bitand)*
static Node *bitxor(Token **rest, Token *tok) {
    Node *node = bitand(&tok, tok);
    while (equal(tok, "^")) {
        node = new_binary(ND_BITXOR, node, NULL, tok);
        node->rhs = bitand(&tok, tok->next);
    }
    *rest = tok;
    return node;
}

// bitand = equality ("&" equality)*
static Node *bitand(Token **rest, Token *tok) {
    Node *node = equality(&tok, tok);
    while (equal(tok, "&")) {
        node = new_binary(ND_BITAND, node, NULL, tok);
        node->rhs = equality(&tok, tok->next);
    }
    *rest = tok;
    return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
    Node *node = relational(&tok, tok);

    for (;;) {
        if (equal(tok, "==")) {
            node = new_binary(ND_EQ, node, NULL, tok);
            node->rhs = relational(&tok, tok->next);
            continue;
        }

        if (equal(tok, "!=")) {
            node = new_binary(ND_NE, node, NULL, tok);
            node->rhs = relational(&tok, tok->next);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
static Node *relational(Token **rest, Token *tok) {
    Node *node = shift(&tok, tok);

    for (;;) {
        if (equal(tok, "<")) {
            node = new_binary(ND_LT, node, NULL, tok);
            node->rhs = shift(&tok, tok->next);
            continue;
        }

        if (equal(tok, "<=")) {
            node = new_binary(ND_LE, node, NULL, tok);
            node->rhs = shift(&tok, tok->next);
            continue;
        }

        if (equal(tok, ">")) {
            node = new_binary(ND_LT, NULL, node, tok);
            node->lhs = shift(&tok, tok->next);
            continue;
        }

        if (equal(tok, ">=")) {
            node = new_binary(ND_LE, NULL, node, tok);
            node->lhs = shift(&tok, tok->next);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// shift = add ("<<" add | ">>" add)*
static Node *shift(Token **rest, Token *tok) {
    Node *node = add(&tok, tok);

    for (;;) {
        if (equal(tok, "<<")) {
            node = new_binary(ND_SHL, node, NULL, tok);
            node->rhs = add(&tok, tok->next);
            continue;
        }

        if (equal(tok, ">>")) {
            node = new_binary(ND_SHR, node, NULL, tok);
            node->rhs = add(&tok, tok->next);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// In C, '+' operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
    add_type(lhs);
    add_type(rhs);

    // num + num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
        return new_binary(ND_ADD, lhs, rhs, tok);

    if (lhs->ty->base && rhs->ty->base)
        error_tok(tok, "invalid operands");

    // Canonicalize `num + ptr` to `ptr + num`
    if (!lhs->ty->base && rhs->ty->base) {
        Node *tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }

    // ptr + num
    rhs = new_binary(ND_MUL, rhs, new_num(size_of(lhs->ty->base), tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
}

// Like `+`, `-` is overloaded for the pointer type
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
    add_type(lhs);
    add_type(rhs);

    // num - num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
        return new_binary(ND_SUB, lhs, rhs, tok);

    // ptr - num
    if (lhs->ty->base && is_integer(rhs->ty)) {
        rhs = new_binary(ND_MUL, rhs, new_num(size_of(lhs->ty->base), tok), tok);
        return new_binary(ND_SUB, lhs, rhs, tok);
    }

    // ptr - ptr, which returns how many elements are between the two
    if (lhs->ty->base && rhs->ty->base) {
        Node *node = new_binary(ND_SUB, lhs, rhs, tok);
        return new_binary(ND_DIV, node, new_num(size_of(lhs->ty->base), tok), tok);
    }

    error_tok(tok, "invalid operands");
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
    Node *node = mul(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (equal(tok, "+")) {
            node = new_add(node, mul(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, "-")) {
            node = new_sub(node, mul(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// mul = cast ("*" cast | "/" cast | "%" cast)*
static Node *mul(Token **rest, Token *tok) {
    Node *node = cast(&tok, tok);

    for (;;) {
        if (equal(tok, "*")) {
            node = new_binary(ND_MUL, node, NULL, tok);
            node->rhs = cast(&tok, tok->next);
            continue;
        }

        if (equal(tok, "/")) {
            node = new_binary(ND_DIV, node, NULL, tok);
            node->rhs = cast(&tok, tok->next);
            continue;
        }

        if (equal(tok, "%")) {
            node = new_binary(ND_MOD, node, NULL, tok);
            node->rhs = cast(&tok, tok->next);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// compound-literal = initializer "}"
static Node *compound_literal(Token **rest, Token *tok, Type *ty, Token *start) {
    if (scope_depth == 0) {
        Var *var = new_gvar(new_label(), ty, true, true);
        var->initializer = gvar_initializer(rest, tok, ty);
        return new_var_node(var, start);
    }

    Var *var = new_lvar(new_label(), ty);
    Node *lhs = new_node(ND_STMT_EXPR, tok);
    lhs->body = lvar_initializer(rest, tok, var)->body;
    Node *rhs = new_var_node(var, tok);
    return new_binary(ND_COMMA, lhs, rhs, tok);
}

// cast = "(" type-name ")" "{" compound_literal
//      | "(" type-name ")" cast
//      | unary
static Node *cast(Token **rest, Token *tok) {
    if (equal(tok, "(") && is_typename(tok->next)) {
        Token *start = tok;
        Type *ty = typename(&tok, tok->next);
        tok = skip(tok, ")");

        if (equal(tok, "{"))
            return compound_literal(rest, tok, ty, start);

        Node *node = new_unary(ND_CAST, NULL, start);
        node->lhs = cast(rest, tok);
        add_type(node->lhs);
        node->ty = ty;
        return node;
    }

    return unary(rest, tok);
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | postfix
static Node *unary(Token **rest, Token *tok) {
    if (equal(tok, "+"))
        return cast(rest, tok->next);

    if (equal(tok, "-"))
        return new_binary(ND_SUB, new_num(0, tok), cast(rest, tok->next), tok);

    if (equal(tok, "&"))
        return new_unary(ND_ADDR, cast(rest, tok->next), tok);

    if (equal(tok, "*"))
        return new_unary(ND_DEREF, cast(rest, tok->next), tok);

    if (equal(tok, "!"))
        return new_unary(ND_NOT, cast(rest, tok->next), tok);

    if (equal(tok, "~"))
        return new_unary(ND_BITNOT, cast(rest, tok->next), tok);

    // Read ++i as i+=1
    if (equal(tok, "++"))
        return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));

    // Read --i as i-=1
    if (equal(tok, "--"))
        return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));

    return postfix(rest, tok);
}

// struct-members = (typespec declarator ("," declarator)* ";")*
static Member *struct_members(Token **rest, Token *tok) {
    Member head = {};
    Member *cur = &head;

    while (!equal(tok, "}")) {
        VarAttr attr = {};
        Type *basety = typespec(&tok, tok, &attr);
        int cnt = 0;

        while (!consume(&tok, tok, ";")) {
            if (cnt++)
                tok = skip(tok, ",");

            Member *mem = calloc(1, sizeof(Member));
            mem->ty = declarator(&tok, tok, basety);
            mem->name = mem->ty->name;
            mem->align = attr.align ? attr.align : mem->ty->align;
            cur = cur->next = mem;
        }
    }

    *rest = tok->next;
    return head.next;
}

// struct-union-decl = ident? ("{" struct-members)?
static Type *struct_union_decl(Token **rest, Token *tok) {
    // Read a tag
    Token *tag = NULL;
    if (tok->kind == TK_IDENT) {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !equal(tok, "{")) {
        *rest = tok;

        TagScope *sc = find_tag(tag);
        if (sc)
            return sc->ty;

        Type *ty = struct_type();
        ty->is_incomplete = true;
        push_tag_scope(tag, ty);
        return ty;
    }

    tok = skip(tok, "{");

    // Construct a struct object
    Type *ty = struct_type();
    ty->members = struct_members(rest, tok);

    if (tag) {
        // If this is a redefinition, overwrite a previous type.
        // Otherwise, register the struct type.
        TagScope *sc = find_tag(tag);
        if (sc && sc->depth == scope_depth) {
            *sc->ty = *ty;
            return sc->ty;
        }

        push_tag_scope(tag, ty);
    }

    return ty;
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
    Type *ty = struct_union_decl(rest, tok);

    // Assign offsets within the struct to members
    int offset = 0;
    for (Member *mem = ty->members; mem; mem = mem->next) {
        offset = align_to(offset, mem->align);
        mem->offset = offset;
        offset += size_of(mem->ty);

        if (ty->align < mem->align)
            ty->align = mem->align;
    }
    ty->size = align_to(offset, ty->align);
    return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
    Type *ty = struct_union_decl(rest, tok);

    // If union, we don't have to assign offsets because they
    // are already initialized to zero. We need to compute the
    // alignment and the size through.
    for (Member *mem = ty->members; mem; mem = mem->next) {
        if (ty->align < mem->align)
            ty->align = mem->align;
        if (ty->size < size_of(mem->ty))
            ty->size = size_of(mem->ty);
    }
    ty->size = align_to(ty->size, ty->align);
    return ty;
}

static Member *get_struct_member(Type *ty, Token *tok) {
    for (Member *mem = ty->members; mem; mem = mem->next)
        if (mem->name->len == tok->len &&
            !strncmp(mem->name->loc, tok->loc, tok->len))
            return mem;

    error_tok(tok, "no such member");
}

static Node *struct_ref(Node *lhs, Token *tok) {
    add_type(lhs);
    if (lhs->ty->kind != TY_STRUCT)
        error_tok(lhs->tok, "not a struct");

    Node *node = new_unary(ND_MEMBER, lhs, tok);
    node->member = get_struct_member(lhs->ty, tok);
    return node;
}

// Convert A++ to `tmp = &A, *tmp = *tmp + 1, *tmp - 1`
// where tmp is a fresh pointer variable.
static Node *new_inc_dec(Node *lhs, Token *tok, bool is_inc) {
    add_type(lhs);
    Var *var = new_lvar("", pointer_to(lhs->ty));
    int addend = is_inc ? 1 : -1;

    Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
                             new_unary(ND_ADDR, lhs, tok), tok);

    Node *expr2 =
        new_binary(ND_ASSIGN,
                   new_unary(ND_DEREF, new_var_node(var, tok), tok),
                   new_add(new_unary(ND_DEREF, new_var_node(var, tok), tok),
                           new_num(addend, tok), tok),
                   tok);

    Node *expr3 = new_add(new_unary(ND_DEREF, new_var_node(var, tok), tok),
                          new_num(-addend, tok), tok);

    return new_binary(ND_COMMA, expr1, new_binary(ND_COMMA, expr2, expr3, tok), tok);
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
static Node *postfix(Token **rest, Token *tok) {
    Node *node = primary(&tok, tok);

    for (;;) {
        if (equal(tok, "[")) {
            //x[y] is short for *(x+y)
            Token *start = tok;
            Node *idx = expr(&tok, tok->next);
            tok = skip(tok, "]");
            node = new_unary(ND_DEREF, new_add(node, idx, start), start);
            continue;
        }

        if (equal(tok, ".")) {
            node = struct_ref(node, tok->next);
            tok = tok->next->next;
            continue;
        }

        if (equal(tok, "->")) {
            // x->y is short for (*x).y
            node = new_unary(ND_DEREF, node, tok);
            node = struct_ref(node, tok->next);
            tok = tok->next->next;
            continue;
        }

        if (equal(tok, "++")) {
            node = new_inc_dec(node, tok, true);
            tok = tok->next;
            continue;
        }

        if (equal(tok, "--")) {
            node = new_inc_dec(node, tok, false);
            tok = tok->next;
            continue;
        }

        *rest = tok;
        return node;
    }
}

// func-args = "(" (assign ("," assign)*)? ")"
static Node *func_args(Token **rest, Token *tok) {
    Node head = {};
    Node *cur = &head;

    while (!equal(tok, ")")) {
        if (cur != &head)
            tok = skip(tok, ",");
        cur = cur->next = assign(&tok, tok);
    }

    *rest = skip(tok, ")");
    return head.next;
}

// primary = "(" "{" stmt stmt* "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | "alignof" "(" type-name ")"
//         | ident args?
//         | str
//         | num
static Node *primary(Token **rest, Token *tok) {
    if (equal(tok, "(") && equal(tok->next, "{")) {
        // This is a GNU statement expression
        Node *node = new_node(ND_STMT_EXPR, tok);
        node->body = compound_stmt(&tok, tok->next->next)->body;
        *rest = skip(tok, ")");

        Node *cur = node->body;
        while (cur->next)
            cur = cur->next;

        if (cur->kind != ND_EXPR_STMT)
            error_tok(cur->tok, "statement expression returning void is not supported");
        return node;
    }

    if (equal(tok, "(")) {
        Node *node = expr(&tok, tok->next);
        *rest = skip(tok, ")");
        return node;
    }

    if (equal(tok, "sizeof") && equal(tok->next, "(") && is_typename(tok->next->next)) {
        Type *ty = typename(&tok, tok->next->next);
        *rest = skip(tok, ")");
        return new_ulong(size_of(ty), tok);
    }

    if (equal(tok, "sizeof")) {
        Node *node = unary(rest, tok->next);
        add_type(node);
        return new_ulong(size_of(node->ty), tok);
    }

    if (equal(tok, "alignof")) {
        tok = skip(tok->next, "(");
        Type *ty = typename(&tok, tok);
        *rest = skip(tok, ")");
        return new_ulong(ty->align, tok);
    }

    if (tok->kind == TK_IDENT) {
        // Function call
        if (equal(tok->next, "(")) {
            Node *node = new_node(ND_FUNCALL, tok);
            VarScope *sc = find_var(tok);

            node->funcname = strndup(tok->loc, tok->len);
            node->args = func_args(rest, tok->next->next);

            if (sc) {
                if (!sc->var || sc->var->ty->kind != TY_FUNC)
                    error_tok(tok, "not a function");
                node->func_ty = sc->var->ty;
            } else {
                warn_tok(node->tok, "implicit declaration of a function");
                node->func_ty = func_type(ty_int);
            }
            return node;
        }

        // Variable or enum constant
        VarScope *sc = find_var(tok);
        if (!sc || (!sc->var && !sc->enum_ty))
            error_tok(tok, "undefined variable");

        Node *node;
        if (sc->var)
            node = new_var_node(sc->var, tok);
        else
            node = new_num(sc->enum_val, tok);

        *rest = tok->next;
        return node;
    }

    if (tok->kind == TK_STR) {
        Var *var = new_string_literal(tok->contents, tok->cont_len);
        *rest = tok->next;
        return new_var_node(var, tok);
    }

    if (tok->kind != TK_NUM)
        error_tok(tok, "expected expression");

    Node *node = new_num(tok->val, tok);
    node->ty = tok->ty;
    *rest = tok->next;
    return node;
}

// program = (funcdef | global-var)*
Program *parse(Token *tok) {
    // Add built-in function types
    new_gvar("__builtin_va_start", func_type(ty_void), false, false);

    // Read source code util EOF
    Function head = {};
    Function *cur = &head;
    globals = NULL;

    while (tok->kind != TK_EOF) {
        Token *start = tok;
        VarAttr attr = {};
        Type *basety = typespec(&tok, tok, &attr);
        if (consume(&tok, tok, ";"))
            continue;
        Type *ty = declarator(&tok, tok, basety);

        // Typedef
        if (attr.is_typedef) {
            for (;;) {
                push_scope(get_ident(ty->name))->type_def = ty;
                if (consume(&tok, tok, ";"))
                    break;
                tok = skip(tok, ",");
                ty = declarator(&tok, tok, basety);
            }
            continue;
        }

        // Function
        if (ty->kind == TY_FUNC) {
            current_fn = new_gvar(get_ident(ty->name), ty, true, false);
            if (!consume(&tok, tok, ";"))
                cur = cur->next = funcdef(&tok, start);
            continue;
        }

        // Global variable
        for (;;) {
            Var *var = new_gvar(get_ident(ty->name), ty, attr.is_static, !attr.is_extern);
            if (attr.align)
                var->align = attr.align;

            if (equal(tok, "="))
                var->initializer = gvar_initializer(&tok, tok->next, ty);

            if (consume(&tok, tok, ";"))
                break;
            tok = skip(tok, ",");
            ty = declarator(&tok, tok, basety);
        }
    }

    Program *prog = calloc(1, sizeof(Program));
    prog->globals = globals;
    prog->fns = head.next;
    return prog;
}
