#include "ee_ir.h"

static const char* _s_op_names[OP_COUNT] = {
	"OP_HALT",

	"OP_ALLOCA",
	"OP_MOV",
	"OP_MOVI",

	"OP_ADD",
	"OP_SUB",
	"OP_MUL",
	"OP_IDIV",

	"OP_OR",
	"OP_AND",
	"OP_XOR",
	"OP_SHL",
	"OP_SHR"
};

VM_Program ee_vm_prog_new(size_t ops_count, size_t consts_count, size_t data_bytes, const Allocator* allocator)
{
	VM_Program out = { 0 };

	if (allocator == NULL)
	{
		out.allocator.alloc_fn = ee_default_alloc;
		out.allocator.realloc_fn = ee_default_realloc;
		out.allocator.free_fn = ee_default_free;
		out.allocator.context = NULL;
	}
	else
	{
		memcpy(&out.allocator, allocator, sizeof(Allocator));
	}

	out.ops = ee_linked_array_new(ops_count, sizeof(VM_Op), &out.allocator);
	out.consts = ee_linked_array_new(consts_count, sizeof(VM_Val), &out.allocator);
	out.data = ee_linked_arena_new(data_bytes, EE_NO_REWIND, &out.allocator);
	out.symbols = ee_dict_def_m(128, VM_Index, Token*);

	return out;
}

VM_Op ee_vm_prog_get_op(VM_Program* prog, size_t i)
{
	VM_Op op = *(VM_Op*)ee_linked_array_at(&prog->ops, i);

	return op;
}

void ee_vm_prog_push_op_3(VM_Program* prog, VM_Op_Code code, VM_Index r_d, VM_Index r_1, VM_Index r_0)
{
	VM_Op op = { 0 };

	op.code = code;
	op.r_d = r_d;
	op.r_1 = r_1;
	op.r_0 = r_0;

	ee_linked_array_push(&prog->ops, EE_RECAST_U8(op));
}

void ee_vm_prog_push_op_2(VM_Program* prog, VM_Op_Code code, VM_Index r_d, VM_Index r_1)
{
	VM_Op op = { 0 };

	op.code = code;
	op.r_d = r_d;
	op.r_1 = r_1;
	op.r_0 = EE_VM_INVALID_REG;

	ee_linked_array_push(&prog->ops, EE_RECAST_U8(op));
}

void ee_vm_prog_push_op_1(VM_Program* prog, VM_Op_Code code, VM_Index r_d)
{
	VM_Op op = { 0 };

	op.code = code;
	op.r_d = r_d;
	op.r_1 = EE_VM_INVALID_REG;
	op.r_0 = EE_VM_INVALID_REG;

	ee_linked_array_push(&prog->ops, EE_RECAST_U8(op));
}

void ee_vm_prog_push_op_0(VM_Program* prog, VM_Op_Code code)
{
	VM_Op op = { 0 };

	op.code = code;
	op.r_d = EE_VM_INVALID_REG;
	op.r_1 = EE_VM_INVALID_REG;
	op.r_0 = EE_VM_INVALID_REG;

	ee_linked_array_push(&prog->ops, EE_RECAST_U8(op));
}

VM_Val ee_vm_prog_const_at(VM_Program* prog, VM_Index i)
{
	VM_Val out = *(VM_Val*)ee_linked_array_at(&prog->consts, i);
	return out;
}

VM_Index ee_vm_prog_push_const(VM_Program* prog, VM_Val const_val)
{
	VM_Index index = ee_linked_array_len(&prog->consts);
	ee_linked_array_push(&prog->consts, EE_RECAST_U8(const_val));

	return index;
}

Virtual_Machine ee_vm_new(size_t stack_size, size_t heap_size, const Allocator* allocator)
{
	Virtual_Machine out = { 0 };

	if (allocator == NULL)
	{
		out.allocator.alloc_fn = ee_default_alloc;
		out.allocator.realloc_fn = ee_default_realloc;
		out.allocator.free_fn = ee_default_free;
		out.allocator.context = NULL;
	}
	else
	{
		memcpy(&out.allocator, allocator, sizeof(Allocator));
	}

	out.stack = ee_linked_array_new(stack_size, sizeof(VM_Val), &out.allocator);
	out.halt = EE_FALSE;

	return out;
}

VM_Val ee_vm_stack_at(Virtual_Machine* vm, VM_Index i)
{
	VM_Val out = *(VM_Val*)ee_linked_array_at(&vm->stack, (size_t)vm->bp + i);
	return out;
}

void ee_vm_stack_set(Virtual_Machine* vm, VM_Index i, VM_Val val)
{
	VM_Val* out = (VM_Val*)ee_linked_array_at(&vm->stack, (size_t)vm->bp + i);
	*out = val;
}

void ee_vm_run(Virtual_Machine* vm, const VM_Program* prog)
{
	while (vm->ip < ee_linked_array_len(&prog->ops) && !vm->halt)
	{
		VM_Op op = ee_vm_prog_get_op(prog, vm->ip);

		switch (op.code)
		{
		case OP_ALLOCA:
		{
			// TODO(eesuck): add bulk alloc or bulk insert zero/none into linked array implementation
			VM_Val none = { 0 };
			
			for (size_t i = 0; i < op.r_d; ++i)
			{
				ee_linked_array_push(&vm->stack, EE_RECAST_U8(none));
			}

			vm->sp += op.r_d;
		} break;
		case OP_MOV:
		{
			VM_Val src = ee_vm_stack_at(vm, op.r_1);
			ee_vm_stack_set(vm, op.r_d, src);
		} break;
		case OP_MOVI:
		{
			VM_Val src = ee_vm_prog_const_at(prog, op.r_1);
			ee_vm_stack_set(vm, op.r_d, src);
		} break;
		case OP_ADD:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 + src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SUB:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 - src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_MUL:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 * src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_IDIV:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 / src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_OR:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 | src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_AND:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 & src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_XOR:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 ^ src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SHL:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 << src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SHR:
		{
			VM_Val src_1 = ee_vm_prog_const_at(prog, op.r_1);
			VM_Val src_0 = ee_vm_prog_const_at(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 >> src_0.as_u64 };

			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_HALT: 
		{
			vm->halt = EE_TRUE; 
		} break;
		default: EE_ASSERT(0, "Unknown op code (%d)", op.code); break;
		}

		vm->ip++;
	}
}

void ee_vm_debug_print(const Virtual_Machine* vm)
{
	EE_PRINTLN("\nIP: (%d), SP: (%d), BP: (%d)", vm->ip, vm->sp, vm->bp);
	EE_PRINTLN("STACK (%d):", vm->sp);
	EE_PRINT("    ");

	for (VM_Index i = 0; i < vm->sp; ++i)
	{
		VM_Val val = *(VM_Val*)ee_linked_array_at(&vm->stack, i);

		EE_PRINT("[0x%X%X]", val.as_dwords[1], val.as_dwords[0]);

		if (i < vm->sp - 1)
		{
			if ((i & 7) == 0 && i != 0)
				EE_PRINT("\n    ");
			else
				EE_PRINT(" ");
		}
		else
		{
			EE_PRINT("\n");
		}
	}
}

void ee_vm_prog_debug_print(const VM_Program* prog)
{
	size_t consts_count = ee_linked_array_len(&prog->consts);

	EE_PRINTLN("CONSTANTS (%zu):", consts_count);
	EE_PRINT("    ");

	for (VM_Index i = 0; i < consts_count; ++i)
	{
		VM_Val val = *(VM_Val*)ee_linked_array_at(&prog->consts, i);

		EE_PRINT("[0x%X%X]", val.as_dwords[1], val.as_dwords[0]);

		if (i < consts_count - 1)
		{
			if (i & 7)
				EE_PRINT("\n    ");
			else
				EE_PRINT(" ");
		}
		else
		{
			EE_PRINT("\n");
		}
	}

	size_t ops_count = ee_linked_array_len(&prog->ops);

	EE_PRINTLN("\nINSTRUCTIONS (%zu):", ops_count);
	for (size_t i = 0; i < ops_count; ++i)
	{
		VM_Op* op = (VM_Op*)ee_linked_array_at((Linked_Array*)&prog->ops, i);

		EE_PRINT("    %04zu: ", i);

		if (op->code < OP_COUNT && _s_op_names[op->code])
			EE_PRINT("%-6s", _s_op_names[op->code]);
		else
			EE_PRINT("OP_%-3d", op->code);

		EE_PRINTLN(" r%-2u, r%-2u, r%-2u", op->r_d, op->r_1, op->r_0);
	}
}