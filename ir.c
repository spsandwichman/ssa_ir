#include "ir.h"

IR_Module* ir_new_module(string name) {
    IR_Module* mod = malloc(sizeof(IR_Module));

    mod->functions = NULL;
    mod->globals   = NULL;
    mod->functions_len = 0;
    mod->globals_len   = 0;

    mod->name = name;

    da_init(&mod->symtab, 4);
    return mod;
}

// if (sym == NULL), create new symbol with no name
IR_Function* ir_new_function(IR_Module* mod, IR_Symbol* sym, bool global) {
    IR_Function* fn = malloc(sizeof(IR_Function));

    fn->sym = sym ? sym : ir_new_symbol(mod, NULL_STR, global, true, fn);
    fn->alloca = arena_make(IR_FN_ALLOCA_BLOCK_SIZE);
    da_init(&fn->blocks, 1);
    fn->entry_idx = 0;
    fn->exit_idx = 0;

    mod->functions = realloc(mod->functions, mod->functions_len+1);
    mod->functions[mod->functions_len++] = fn;
}

// if (sym == NULL), create new symbol with no name
IR_Global* ir_new_global(IR_Module* mod, IR_Symbol* sym, bool global, bool read_only, bool zeroed) {
    IR_Global* gl = malloc(sizeof(IR_Global));

    gl->sym = sym ? sym : ir_new_symbol(mod, NULL_STR, global, false, gl);
    gl->read_only = read_only;
    gl->zeroed = zeroed;
    gl->data = NULL;
    gl->data_len = 0;

    mod->globals = realloc(mod->globals, mod->globals_len+1);
    mod->globals[mod->globals_len++] = gl;
}

void ir_set_global_data(IR_Global* global, type* T, u8* data, u32 data_len) {
    global->T = T;
    global->data = data;
    global->data_len = data_len;
}

// WARNING: does NOT check if a symbol already exists
// in most cases, use ir_find_or_create_symbol
IR_Symbol* ir_new_symbol(IR_Module* mod, string name, u8 tag, bool function, void* ref) {
    IR_Symbol* sym = malloc(sizeof(IR_Symbol));
    sym->name = name;
    sym->ref = ref;
    sym->is_function = function;
    sym->tag = tag;

    da_append(&mod->symtab, sym);
}

// use this instead of ir_new_symbol
IR_Symbol* ir_find_or_new_symbol(IR_Module* mod, string name, u8 tag, bool function, void* ref) {
    IR_Symbol* sym = ir_find_symbol(mod, name);
    return sym ? sym : ir_new_symbol(mod, name, tag, function, ref);
}

IR_Symbol* ir_find_symbol(IR_Module* mod, string name) {
    FOR_URANGE(i, 0, mod->symtab.len) {
        if (string_eq(mod->symtab.at[i]->name, name)) {
            return mod->symtab.at[i];
        }
    }
    return NULL;
}

IR_BasicBlock* ir_new_basic_block() {
    TODO("");
}

IR* ir_make(IR_Function* f, u8 type) {
    if (type > IR_INSTR_COUNT) type = IR_INVALID;
    IR* ir = arena_alloc(&f->alloca, ir_sizes[type], 8);
    ir->tag = type;
    return ir;
}

const size_t ir_sizes[] = {
    [IR_INVALID]    = 0,
    [IR_ELIMINATED] = 0,

    [IR_ADD] = sizeof(IR_BinOp),
    [IR_SUB] = sizeof(IR_BinOp),
    [IR_MUL] = sizeof(IR_BinOp),
    [IR_DIV] = sizeof(IR_BinOp),
    
    [IR_AND]   = sizeof(IR_BinOp),
    [IR_OR]    = sizeof(IR_BinOp),
    [IR_NOR]   = sizeof(IR_BinOp),
    [IR_XOR]   = sizeof(IR_BinOp),
    [IR_SHL]   = sizeof(IR_BinOp),
    [IR_SHR]   = sizeof(IR_BinOp),
    [IR_TRUNC] = sizeof(IR_BinOp),
    [IR_SEXT]  = sizeof(IR_BinOp),
    [IR_ZEXT]  = sizeof(IR_BinOp),

    [IR_STACKALLOC] = sizeof(IR_StackAlloc),

    [IR_LOAD]     = sizeof(IR_Load),
    [IR_VOL_LOAD] = sizeof(IR_Load),

    [IR_STORE]     = sizeof(IR_Store),
    [IR_VOL_STORE] = sizeof(IR_Store),

    [IR_CONST]      = sizeof(IR_Const),
    [IR_LOADSYMBOL] = sizeof(IR_LoadSymbol),

    [IR_MOV] = sizeof(IR_Mov),
    [IR_PHI] = sizeof(IR_Phi),

    [IR_BRANCH] = sizeof(IR_Branch),
    [IR_JUMP]   = sizeof(IR_Jump),

    [IR_GETPARAM]  = sizeof(IR_GetParam),
    [IR_SETRETURN] = sizeof(IR_SetReturn),

    [IR_RETURN] = sizeof(IR_Return),
};

IR_BinOp* ir_make_binop(IR_Function* f, IR* lhs, IR* rhs, u8 type) {
    IR_BinOp* ir = (IR_BinOp*) ir_make(f, type);
    
    ir->lhs = lhs;
    ir->rhs = rhs;
    return ir;
}

IR_Cast* ir_make_cast(IR_Function* f, IR* source, type* to) {
    IR_Cast* ir = (IR_Cast*) ir_make(f, IR_CAST);
    ir->source = source;
    ir->to = to;
    return ir;
}

IR_StackAlloc* ir_make_stackalloc(IR_Function* f, u32 size, u32 align, type* T) {
    IR_StackAlloc* ir = (IR_StackAlloc*) ir_make(f, IR_STACKALLOC);
    
    ir->size = size;
    ir->align = align;
    ir->T = T;
    return ir;
}

IR_Load* ir_make_load(IR_Function* f, IR* location, type* T, bool is_vol) {
    IR_Load* ir = (IR_Load*) ir_make(f, IR_LOAD);

    if (is_vol) ir->base.tag = IR_VOL_LOAD;
    ir->location = location;
    ir->T = T;
    return ir;
}

IR_Store* ir_make_store(IR_Function* f, IR* location, IR* value, type* T, bool is_vol) {
    IR_Store* ir = (IR_Store*) ir_make(f, IR_STORE);
    
    if (is_vol) ir->base.tag = IR_VOL_STORE;
    ir->location = location;
    ir->value = value;
    ir->T = T;
    return ir;
}

IR_Const* ir_make_const(IR_Function* f, type* T) {
    IR_Const* ir = (IR_Const*) ir_make(f, IR_CONST);
    ir->T = T;
    return ir;
}

IR_LoadSymbol* ir_make_loadsymbol(IR_Function* f, IR_Symbol* symbol, type* T) {
    IR_LoadSymbol* ir = (IR_LoadSymbol*) ir_make(f, IR_LOADSYMBOL);
    ir->sym = symbol;
    ir->T = T;
    return ir;
}

IR_Mov* ir_make_mov(IR_Function* f, IR* source) {
    IR_Mov* ir = (IR_Mov*) ir_make(f, IR_MOV);
    ir->source = source;
    return ir;
}

// use in the format (f, source_count, source_1, source_BB_1, source_2, source_BB_2, ...)
IR_Phi* ir_make_phi(IR_Function* f, u16 count, ...) {
    IR_Phi* ir = (IR_Phi*) ir_make(f, IR_PHI);
    ir->len = count;

    ir->sources    = malloc(sizeof(*ir->sources) * count);
    ir->source_BBs = malloc(sizeof(*ir->source_BBs) * count);

    va_list args;
    va_start(args, count);
    FOR_RANGE(i, 0, count) {
        ir->sources[i]    = va_arg(args, IR*);
        ir->source_BBs[i] = va_arg(args, IR_BasicBlock*);
    }
    va_end(args);

    return ir;
}

void ir_add_phi_source(IR_Phi* phi, IR* source, IR_BasicBlock* source_block) {
    // wrote this and then remembered realloc exists. too late :3
    IR** new_sources    = malloc(sizeof(*phi->sources) * (phi->len + 1));
    IR_BasicBlock** new_source_BBs = malloc(sizeof(*phi->source_BBs) * (phi->len + 1));

    if (!new_sources || !new_source_BBs) {
        CRASH("malloc returned null");
    }

    memcpy(new_sources, phi->sources, sizeof(*phi->sources) * phi->len);
    memcpy(new_source_BBs, phi->source_BBs, sizeof(*phi->source_BBs) * phi->len);

    new_sources[phi->len]    = source;
    new_source_BBs[phi->len] = source_block;

    free(phi->sources);
    free(phi->source_BBs);

    phi->sources = new_sources;
    phi->source_BBs = new_source_BBs;
    phi->len++;
}

IR_Jump* ir_make_jump(IR_Function* f, IR_BasicBlock* dest) {
    IR_Jump* ir = (IR_Jump*) ir_make(f, IR_JUMP);
    ir->dest = dest;
    return ir;
}

IR_Branch* ir_make_branch(IR_Function* f, u8 cond, IR* lhs, IR* rhs, IR_BasicBlock* if_true, IR_BasicBlock* if_false) {
    IR_Branch* ir = (IR_Branch*) ir_make(f, IR_BRANCH);
    ir->cond = cond;
    ir->lhs = lhs;
    ir->rhs = rhs;
    ir->if_true  = if_true;
    ir->if_false = if_false;
    return ir;
}

IR_GetParam* ir_make_getparam(IR_Function* f, u16 param) {
    IR_GetParam* ir = (IR_GetParam*) ir_make(f, IR_GETPARAM);
    ir->param_idx = param;
    return ir;
}

IR_SetReturn* ir_make_setreturn(IR_Function* f, u16 param, IR* source) {
    IR_SetReturn* ir = (IR_SetReturn*) ir_make(f, IR_SETRETURN);
    ir->return_idx = param;
    ir->source = source;
    return ir;
}

IR_Return* ir_make_return(IR_Function* f) {
    return (IR_Return*) ir_make(f, IR_RETURN);
}