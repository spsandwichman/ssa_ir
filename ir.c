#include "ir.h"

IR_Module* ir_init_module() {

}

IR* ir_make(IR_Function* f, u8 type) {
    if (type > IR_INSTR_COUNT) type = IR_INVALID;
    IR* ir = arena_alloc(&f->alloca, ir_sizes[type], 8);
    ir->tag = type;
    return ir;
}

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