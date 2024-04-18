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
    IR_ELIMINATED, // an IR element that has been "deleted".

    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,

    IR_GETFIELDPTR,

    IR_STACKALLOC,

    IR_LOAD,
    IR_VOL_LOAD,

    IR_STORE,
    IR_VOL_STORE,

    IR_CONST,
    IR_SYMBOL,

    IR_MOV,
    IR_PHI,

    IR_BRANCH,
    IR_JUMP,

    IR_GETPARAM,
    IR_SETRETURN,

    IR_RET,
};

// basic IR structure, common with every node
typedef struct IR {
    u8 tag;
} IR;

typedef struct IR_BinOp {
    IR base;

    IR* lhs;
    IR* rhs;
} IR_BinOp;

typedef struct IR_StackAlloc {
    IR base;

    u32 size;
    u32 align;
    type* T;
} IR_StackAlloc;

typedef struct IR_GetFieldPtr {
    IR base;

    u32 index;

    IR* ptr;

    type* T;
} IR_GetFieldPtr;

typedef struct IR_Load {
    IR base;

    IR* location;
} IR_Load;

typedef struct IR_Store {
    IR base;

    IR* location;
    IR* value;
} IR_Store;

typedef struct IR_Const {
    IR base;

    union {
        i8  i8;
        i16 i16;
        i32 i32;
        i64 i64;

        u8  u8;
        u16 u16;
        u32 u32;
        u64 u64;

        f16 f16;
        f32 f32;
        f64 f64;
    };
    type* T;
} IR_Const;

typedef struct IR_Symbol {
    IR base;
    
    string symbol;
} IR_Symbol;

typedef struct IR_Mov {
    IR base;

    IR* source;
} IR_Mov;

typedef struct IR_Phi {
    IR base;

    IR** sources;
    BB** source_BBs;
    u16 len;
} IR_Phi;

typedef struct IR_Jump {
    IR base;

    BB* destination;
} IR_Jump;

enum {
    COND_LT,    // <
    COND_GT,    // >
    COND_LE,    // >=
    COND_GE,    // <=
    COND_EQ,    // ==
    COND_NE,    // !=
};

typedef struct IR_Branch {
    IR base;

    BB* if_true;
    BB* if_false;
    IR* lhs;
    IR* rhs;
} IR_Branch;

typedef struct IR_GetParam {
    IR base;

    u16 param_idx;
} IR_GetParam;

typedef struct IR_SetReturn {
    IR base;

    u16 return_idx;
} IR_SetReturn;

typedef struct IR_Ret {
    IR base;
} IR_Ret;

extern const size_t ir_sizes[];