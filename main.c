#define ORBIT_IMPLEMENTATION

#include "ir.h"

int main() {

    IR_Module* mod = ir_new_module(str("test"));
    
    IR_Function* fn = ir_new_function(mod, NULL, true);
    fn->sym->name = str("add");
    ir_set_func_params(fn, 2, make_type(TYPE_I64), make_type(TYPE_I64));
    ir_set_func_returns(fn, 1, make_type(TYPE_I64));

    IR_BasicBlock* bb = ir_new_basic_block(fn, str("BB1"));
    ir_add(bb, ir_make_paramval(fn, 0));
    ir_add(bb, ir_make_paramval(fn, 1));
    ir_add(bb, ir_make_binop(fn, IR_ADD, bb->at[0], bb->at[1]));
    ir_add(bb, ir_make_returnval(fn, 0, bb->at[2]));
    ir_add(bb, ir_make_return(fn));

    ir_type_resolve_all(mod, fn, true);

    ir_print_function(fn);
}