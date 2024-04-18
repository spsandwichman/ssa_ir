#include "ir.h"

IR* ir_make(u8 type) {
    IR* ir = malloc(ir_sizes[type]);
    ir->tag = type;
    return ir;
}

IR_BinOp* ir_make_binop(IR* lhs, IR* rhs, u8 binop_type) {
    IR_BinOp* ir = (IR_BinOp*) ir_make(binop_type);
    ir->lhs = lhs;
    ir->rhs = rhs;
}

const size_t ir_sizes[] = {
    [IR_INVALID] = 0,
    [IR_ELIMINATED] = 0,

    [IR_ADD] = sizeof(IR_BinOp),
    [IR_SUB] = sizeof(IR_BinOp),
    [IR_MUL] = sizeof(IR_BinOp),
    [IR_DIV] = sizeof(IR_BinOp),

    [IR_GETFIELDPTR] = sizeof(IR_GetFieldPtr),

    [IR_STACKALLOC] = sizeof(IR_StackAlloc),

    [IR_LOAD]     = sizeof(IR_Load),
    [IR_VOL_LOAD] = sizeof(IR_Load),

    [IR_STORE]     = sizeof(IR_Store),
    [IR_VOL_STORE] = sizeof(IR_Store),

    [IR_CONST]  = sizeof(IR_Const),
    [IR_SYMBOL] = sizeof(IR_Symbol),

    [IR_MOV] = sizeof(IR_Mov),
    [IR_PHI] = sizeof(IR_Phi),

    [IR_BRANCH] = sizeof(IR_Branch),
    [IR_JUMP]   = sizeof(IR_Jump),

    [IR_GETPARAM]  = sizeof(IR_GetParam),
    [IR_SETRETURN] = sizeof(IR_SetReturn),

    [IR_RET] = sizeof(IR_Ret),
};
