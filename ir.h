#pragma once

#include "orbit.h"
#include "type.h"
#include "arena.h"

typedef struct IR IR;
typedef struct IR_BasicBlock IR_BasicBlock;
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
    IR_SYM_LOCAL,
    IR_SYM_GLOBAL,
};

typedef struct IR_Symbol {
    string name;
    union {
        void* ref;
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

#define IR_FN_ALLOCA_BLOCK_SIZE 0x1000

typedef struct IR_Function {
    IR_Symbol* sym;

    struct {
        IR_BasicBlock** at;
        u32 len;
        u32 cap;
    } blocks;

    u32 entry_idx;
    u32 exit_idx;

    arena alloca;
} IR_Function;

typedef struct IR_BasicBlock {
    IR** at;
    u64 len;
    u64 cap;

    string name;
} IR_BasicBlock;

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
    IR_BasicBlock** source_BBs;
    u16 len;
} IR_Phi;

typedef struct IR_Jump {
    IR base;

    IR_BasicBlock* dest;
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

    IR_BasicBlock* if_true;
    IR_BasicBlock* if_false;
} IR_Branch;

// get value from register parameter OR the pointer to a stack parameter.
// if register, lifetime of the register starts from the start of the entry 
// basic block and continues to this node.
// MUST BE THE FIRST INSTRUCTION IN THE ENTRY BLOCK OR IN A SEQUENCE OF 
// OTHER IR_GetParam INSTRUCTIONS
// BECAUSE OF HOW THE REG ALLOCATOR USES THIS TO CONSTRUCT MACHINE REGISTER LIFETIMES
// ALONGSIDE VIRTUAL REGISTER LIFETIMES
typedef struct IR_GetParam {
    IR base;

    u16 param_idx;
} IR_GetParam;

// set register return val
// can also be used as a source, for retrieving a pointer
// IF SOURCE != NULL, IT MUST BE BEFORE AN IR_Return OR ANOTHER IR_SetReturn INSTRUCTION
// SAME REASON AS ABOVE
typedef struct IR_SetReturn {
    IR base;

    u16 return_idx;
    IR* source; // can be NULL if referring to a stack return val
} IR_SetReturn;

typedef struct IR_Return {
    IR base;
} IR_Return;

extern const size_t ir_sizes[];

IR_Module*   ir_new_module(string name);
IR_Function* ir_new_function(IR_Module* mod, IR_Symbol* sym, bool global);
IR_Global*   ir_new_global(IR_Module* mod, IR_Symbol* sym, bool global, bool read_only, bool zeroed);
IR_Symbol*   ir_new_symbol(IR_Module* mod, string name, u8 tag, bool function, void* ref);
IR_Symbol*   ir_find_symbol(IR_Module* mod, string name);
IR_Symbol*   ir_find_or_new_symbol(IR_Module* mod, string name, u8 tag, bool function, void* ref);

void ir_set_global_data(IR_Global* global, type* T, u8* data, u32 data_len);

IR*            ir_make(IR_Function* f, u8 type);
IR_BinOp*      ir_make_binop(IR_Function* f, IR* lhs, IR* rhs, u8 type);
IR_Cast*       ir_make_cast(IR_Function* f, IR* source, type* to);
IR_StackAlloc* ir_make_stackalloc(IR_Function* f, u32 size, u32 align, type* T);
IR_Load*       ir_make_load(IR_Function* f, IR* location, type* T, bool is_vol);
IR_Store*      ir_make_store(IR_Function* f, IR* location, IR* value, type* T, bool is_vol);
IR_Const*      ir_make_const(IR_Function* f, type* T);
IR_LoadSymbol* ir_make_loadsymbol(IR_Function* f, IR_Symbol* symbol, type* T);
IR_Mov*        ir_make_mov(IR_Function* f, IR* source);
IR_Phi*        ir_make_phi(IR_Function* f, u16 count, ...);
IR_Jump*       ir_make_jump(IR_Function* f, IR_BasicBlock* dest);
IR_Branch*     ir_make_branch(IR_Function* f, u8 cond, IR* lhs, IR* rhs, IR_BasicBlock* if_true, IR_BasicBlock* if_false);
IR_GetParam*   ir_make_getparam(IR_Function* f, u16 param);
IR_SetReturn*  ir_make_setreturn(IR_Function* f, u16 param, IR* source);
IR_Return*     ir_make_return(IR_Function* f);

void ir_add_phi_source(IR_Phi* phi, IR* source, IR_BasicBlock* source_block);