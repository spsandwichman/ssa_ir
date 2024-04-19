#pragma once

#include "orbit.h"
#include "type.h"
#include "arena.h"

typedef struct IR IR;
typedef struct BB BB;
typedef struct CFG CFG;
typedef struct IR_Module IR_Module;
typedef struct IR_Symbol IR_Symbol;
typedef struct IR_Global IR_Global;
typedef struct IR_Function IR_Function;

typedef struct IR_Module {
    IR_Function** functions;
    IR_Global** globals;

    u32 functions_len;
    u32 globals_len;

    string name;

    struct {
        IR_Symbol** at;
        size_t len;
        size_t cap;
    } symtab;

} IR_Module;

enum {
    IR_SYM_GLOBAL,
    IR_SYM_LOCAL,
};

typedef struct IR_Symbol {
    string name;
    union {
        IR_Function* function;
        IR_Global* global;
    };
    bool is_function;
    u8 tag;
} IR_Symbol;

typedef struct IR_Global {
    type* T;

    IR_Symbol* sym;

    u8* data;
    u32 data_len;

    bool zeroed;
    bool read_only;
} IR_Global;

typedef struct IR_Function {
    IR_Symbol* sym;

    struct {
        BB** at;
        u32 len;
        u64 cap;
    } blocks;

    u32 entry_idx;
    u32 exit_idx;

    arena alloca;
} IR_Function;

typedef struct BB {
    IR** at;
    u64 len;
    u64 cap;

    string name;
} BB;

enum {
    IR_INVALID,
    IR_ELIMINATED, // an IR element that has been "deleted".

    // IR_BinOp
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,

    // IR_BinOp
    IR_AND,
    IR_OR,
    IR_NOR,
    IR_XOR,
    IR_SHL,
    IR_SHR,
    IR_TRUNC,
    IR_SEXT,
    IR_ZEXT,

    // IR_Cast
    IR_CAST,

    // IR_StackAlloc
    IR_STACKALLOC,

    // IR_Load
    IR_LOAD,
    IR_VOL_LOAD,

    // IR_Store
    IR_STORE,
    IR_VOL_STORE,

    // IR_Const
    IR_CONST,
    // IR_LoadSymbol
    IR_LOADSYMBOL,

    // IR_Mov
    IR_MOV,
    // IR_Phi
    IR_PHI,

    // IR_Branch
    IR_BRANCH,
    // IR_Jump
    IR_JUMP,

    // IR_GetParam
    IR_GETPARAM,

    // IR_SetReturn
    IR_SETRETURN,

    // IR_Return
    IR_RETURN,

    IR_INSTR_COUNT,
};

// basic IR structure
typedef struct IR {
    u32 number;
    u8 tag;
} IR;

typedef struct IR_BinOp {
    IR base;

    IR* lhs;
    IR* rhs;
} IR_BinOp;

typedef struct IR_Cast {
    IR base;

    type* to;
    IR* source;
} IR_Cast;

typedef struct IR_StackAlloc {
    IR base;

    u32 size;
    u32 align;
    type* T;
} IR_StackAlloc;

typedef struct IR_Load {
    IR base;

    IR* location;
    type* T;
} IR_Load;

typedef struct IR_Store {
    IR base;

    IR* location;
    IR* value;
    type* T;
} IR_Store;

typedef struct IR_Const {
    IR base;

    union {
        bool bool;
        
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

typedef struct IR_LoadSymbol {
    IR base;
    
    IR_Symbol* sym;
    type* T;
} IR_LoadSymbol;

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

    u8 cond;

    IR* lhs;
    IR* rhs;

    BB* if_true;
    BB* if_false;
} IR_Branch;

typedef struct IR_GetParam {
    IR base;

    u16 param_idx;
} IR_GetParam;

typedef struct IR_SetReturn {
    IR base;

    u16 return_idx;
    IR* source;
} IR_SetReturn;

typedef struct IR_Return {
    IR base;
} IR_Return;

extern const size_t ir_sizes[];