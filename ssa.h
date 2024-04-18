#pragma once

#include "orbit.h"
#include "type.h"
#include "exactval.h"

typedef struct IR IR;
typedef struct BB BB;
typedef struct CFG CFG;
typedef struct IR_Function IR_Function;

typedef struct BB {
    IR** at;
    u64 len;
    u64 cap;
} BB;

typedef struct IR_Function {
    BB** blocks;
    u32 len;

    u32 entry_idx;
    u32 exit_idx;
} IR_Function;


enum {
    IR_INVALID,
    IR_ELIMINATED,

    IR_ADD,

    IR_STACKALLOC,

    IR_LOAD,
    IR_STORE,

    IR_CONST,

    IR_MOV,
    IR_PHI,

    IR_BRANCH,
    IR_JUMP,

    IR_GETPARAM,
    IR_SETRETURN,

    IR_RET,
};

typedef struct IR {
    void* content;
    u8 tag;
} IR;

#define CONTENT(ir, T) ((T*) ir.content)

typedef struct IR_Add {
    IR* lhs;
    IR* rhs;

} Ir_Add;

typedef struct IR_StackAlloc {
    u16 size;
    u16 align;
    type* T;
} Ir_StackAlloc;

typedef struct IR_Load {
    IR* location;

    bool is_volatile;
} IR_Load;

typedef struct IR_Store {
    IR* location;
    IR* value;

    bool is_volatile;
} IR_Store;

typedef struct IR_Const {
    exact_value val;
} IR_Const;

typedef struct IR_Mov {
    IR* source;
} IR_Mov;

typedef struct IR_Phi {
    IR** sources;
    BB** source_BBs;
    u16 len;
} IR_Phi;

typedef struct IR_GetParam {
    u16 param_idx;
} IR_GetParam;

typedef struct IR_SetReturn {
    u16 return_idx;
} IR_SetReturn;

typedef struct IR_Ret {
} IR_Ret;