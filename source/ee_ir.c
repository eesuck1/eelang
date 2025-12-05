#include "ee_ir.h"

static const char* _s_op_names[OP_COUNT] = {
	[OP_HALT]      = "OP_HALT",
				   
	[OP_ALLOCAI]   = "OP_ALLOCAI",
	[OP_MOV]       = "OP_MOV",
	[OP_MOVI]      = "OP_MOVI",
				   
	[OP_ADD]       = "OP_ADD",
	[OP_ADDI]      = "OP_ADDI",
	[OP_SUB]       = "OP_SUB",
	[OP_SUBI]      = "OP_SUBI",
	[OP_MUL]       = "OP_MUL",
	[OP_MULI]      = "OP_MULI",
	[OP_IDIV]      = "OP_IDIV",
	[OP_IDIVI]     = "OP_IDIVI",
	[OP_UDIV]      = "OP_UDIV",
	[OP_UDIVI]     = "OP_UDIVI",
	[OP_IMOD]      = "OP_IMOD",
	[OP_IMODI]     = "OP_IMODI",
	[OP_UMOD]      = "OP_UMOD",
	[OP_UMODI]     = "OP_UMODI",
				   
	[OP_FADD]      = "OP_FADD",
	[OP_FADDI]     = "OP_FADDI",
	[OP_FSUB]      = "OP_FSUB",
	[OP_FSUBI]     = "OP_FSUBI",
	[OP_FMUL]      = "OP_FMUL",
	[OP_FMULI]     = "OP_FMULI",
	[OP_FDIV]      = "OP_FDIV",
	[OP_FDIVI]     = "OP_FDIVI",
				   
	[OP_DADD]      = "OP_DADD",
	[OP_DADDI]     = "OP_DADDI",
	[OP_DSUB]      = "OP_DSUB",
	[OP_DSUBI]     = "OP_DSUBI",
	[OP_DMUL]      = "OP_DMUL",
	[OP_DMULI]     = "OP_DMULI",
	[OP_DDIV]      = "OP_DDIV",
	[OP_DDIVI]     = "OP_DDIVI",
				   
	[OP_NOT]       = "OP_NOT",
	[OP_OR]        = "OP_OR",
	[OP_ORI]       = "OP_ORI",
	[OP_AND]       = "OP_AND",
	[OP_ANDI]      = "OP_ANDI",
	[OP_XOR]       = "OP_XOR",
	[OP_XORI]      = "OP_XORI",
	[OP_SHL]       = "OP_SHL",
	[OP_SHLI]      = "OP_SHLI",
	[OP_SHR]       = "OP_SHR",
	[OP_SHRI]      = "OP_SHRI",
	[OP_SAR]       = "OP_SAR",
	[OP_SARI]      = "OP_SARI",
	[OP_SAL]       = "OP_SAL",
	[OP_SALI]      = "OP_SALI",
				   
	[OP_JMP]       = "OP_JMP",
	[OP_JMPI]      = "OP_JMPI",
	[OP_JIZ]       = "OP_JIZ",
	[OP_JIZI]      = "OP_JIZI",
	[OP_JNZ]       = "OP_JNZ",
	[OP_JNZI]      = "OP_JNZI",
				   
	[OP_SEQ]       = "OP_SEQ",
	[OP_SEQI]      = "OP_SEQI",
	[OP_SNEQ]      = "OP_SNEQ",
	[OP_SNEQI]     = "OP_SNEQI",
	[OP_SLEQ]      = "OP_SLEQ",
	[OP_SLEQI]     = "OP_SLEQI",
	[OP_SGEQ]      = "OP_SGEQ",
	[OP_SGEQI]     = "OP_SGEQI",
	[OP_SLT]       = "OP_SLT",
	[OP_SLTI]      = "OP_SLTI",
	[OP_SGT]       = "OP_SGT",
	[OP_SGTI]      = "OP_SGTI",
				   
	[OP_SLEQU]     = "OP_SLEQU",
	[OP_SLEQUI]    = "OP_SLEQUI",
	[OP_SGEQU]     = "OP_SGEQU",
	[OP_SGEQUI]    = "OP_SGEQUI",
	[OP_SLTU]      = "OP_SLTU",
	[OP_SLTUI]     = "OP_SLTUI",
	[OP_SGTU]      = "OP_SGTU",
	[OP_SGTUI]     = "OP_SGTUI",
				   
	[OP_ITOF]      = "OP_ITOF",
	[OP_FTOI]      = "OP_FTOI",
	[OP_ITOD]      = "OP_ITOD",
	[OP_DTOI]      = "OP_DTOI",
	[OP_FTOD]      = "OP_FTOD",
	[OP_DTOF]      = "OP_DTOF",
	[OP_SEXT8]     = "OP_SEXT8",
	[OP_SEXT16]    = "OP_SEXT16",
	[OP_SEXT32]    = "OP_SEXT32",
				   
	[OP_CALL]      = "OP_CALL",
	[OP_CALLI]     = "OP_CALLI",
	[OP_RET]       = "OP_RET",
				   
	[OP_LOAD64]	   = "OP_LOAD64",
	[OP_LOAD32]	   = "OP_LOAD32",
	[OP_LOAD16]	   = "OP_LOAD16",
	[OP_LOAD8]	   = "OP_LOAD8",
				   
	[OP_STORE64]   = "OP_STORE64",
	[OP_STORE32]   = "OP_STORE32",
	[OP_STORE16]   = "OP_STORE16",
	[OP_STORE8]	   = "OP_STORE8",
				   
	[OP_STORE64I]  = "OP_STORE64I",
	[OP_STORE32I]  = "OP_STORE32I",
	[OP_STORE16I]  = "OP_STORE16I",
	[OP_STORE8I]   = "OP_STORE8I",
				   
	[OP_MEMCPY]	   = "OP_MEMCPY",
	[OP_MEMCPYI]   = "OP_MEMCPYI",
				   
	[OP_MALLOC]    = "OP_MALLOC",
	[OP_MALLOCI]   = "OP_MALLOCI",
				   
	[OP_MEMSET]    = "OP_MEMSET",
	[OP_MEMSETI]   = "OP_MEMSETI",
				   
	[OP_REGADDR]   = "OP_REGADDR",
				   
	[OP_REGSET8]   = "OP_REGSET8",
	[OP_REGSET16]  = "OP_REGSET16",
	[OP_REGSET32]  = "OP_REGSET32",
	[OP_REGSET8I]  = "OP_REGSET8I",
	[OP_REGSET16I] = "OP_REGSET16I",
	[OP_REGSET32I] = "OP_REGSET32I",

	[OP_REGCPY8]   = "OP_REGCPY8",
	[OP_REGCPY16]  = "OP_REGCPY16",
	[OP_REGCPY32]  = "OP_REGCPY32",
	[OP_REGCPY8I]  = "OP_REGCPY8I",
	[OP_REGCPY16I] = "OP_REGCPY16I",
	[OP_REGCPY32I] = "OP_REGCPY32I",

	[OP_ASSERT]    = "OP_ASSERT",
	[OP_LOGSTATE]  = "OP_LOGSTATE",
};

static const char* _s_atom_names[ATOM_COUNT] = {
	[ATOM_U8]   = "ATOM_U8",
	[ATOM_U16]  = "ATOM_U16",
	[ATOM_U32]  = "ATOM_U32",
	[ATOM_U64]  = "ATOM_U64",

	[ATOM_I8]   = "ATOM_I8",
	[ATOM_I16]  = "ATOM_I16",
	[ATOM_I32]  = "ATOM_I32",
	[ATOM_I64]  = "ATOM_I64",

	[ATOM_F32]  = "ATOM_F32",
	[ATOM_F64]  = "ATOM_F64",

	[ATOM_BOOL] = "ATOM_BOOL",
	[ATOM_VOID] = "ATOM_VOID",
	[ATOM_TYPE] = "ATOM_TYPE",
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
	out.symbols = ee_dict_def_m(128, VM_Word, Token*);

	return out;
}

VM_Op ee_vm_prog_get_op(VM_Program* prog, size_t i)
{
	VM_Op op = *(VM_Op*)ee_linked_array_at(&prog->ops, i);

	return op;
}

VM_Word ee_vm_prog_make_const(VM_Program* prog, i64 val)
{
	VM_Word out = EE_VM_INVALID_REG;

	if (val >= EE_VM_IMM_MAX || val <= EE_VM_IMM_MIN)
		out = ee_vm_prog_push_const(prog, (VM_Val) { .as_i64 = val });
	else
		out = val | EE_VM_IMM_FLAG;

	return out;
}

void ee_vm_prog_push_op(VM_Program* prog, VM_Op_Code code, VM_Word r_d, VM_Word r_1, VM_Word r_0)
{
	VM_Op op = { 0 };

	op.code = code;
	op.r_d = r_d;
	op.r_1 = r_1;
	op.r_0 = r_0;

	ee_linked_array_push(&prog->ops, EE_RECAST_U8(op));
}

VM_Val ee_vm_prog_const_at(VM_Program* prog, VM_Word i)
{
	VM_Val out = *(VM_Val*)ee_linked_array_at(&prog->consts, i);
	return out;
}

VM_Val ee_vm_prog_get_imm(VM_Program* prog, VM_Word val)
{
	VM_Val out = { .as_u64 = 0 };

	if (val & EE_VM_IMM_FLAG)
		out.as_i64 = val & EE_VM_IMM_SIGN ? (i64)((i32)val) : val & EE_VM_IMM_MASK;
	else
		out = ee_vm_prog_const_at(prog, val & EE_VM_IMM_MASK);

	return out;
}

VM_Word ee_vm_prog_push_const(VM_Program* prog, VM_Val const_val)
{
	VM_Word index = ee_linked_array_len(&prog->consts);
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
	out.frames = ee_linked_array_new(EE_VM_CALL_FRAME_BASE_SIZE, sizeof(VM_Call_Frame), &out.allocator);
	out.heap = ee_linked_arena_new(heap_size, EE_NO_REWIND, &out.allocator);
	out.halt = EE_FALSE;
	out.ticks = 0;

	return out;
}

VM_Val ee_vm_stack_at(Virtual_Machine* vm, VM_Word i)
{
	VM_Val out = *(VM_Val*)ee_linked_array_at(&vm->stack, (size_t)vm->bp + i);
	return out;
}

VM_Val* ee_vm_stack_at_ptr(Virtual_Machine* vm, VM_Word i)
{
	VM_Val* out = (VM_Val*)ee_linked_array_at(&vm->stack, (size_t)vm->bp + i);
	return out;
}

void ee_vm_stack_set(Virtual_Machine* vm, VM_Word i, VM_Val val)
{
	VM_Val* out = (VM_Val*)ee_linked_array_at(&vm->stack, (size_t)vm->bp + i);
	*out = val;
}

void ee_vm_run(Virtual_Machine* vm, const VM_Program* prog)
{
	while (vm->ip < ee_linked_array_len(&prog->ops) && !vm->halt)
	{
		VM_Op op = ee_vm_prog_get_op(prog, vm->ip);

		vm->ticks++;
		vm->ip++;

		switch (op.code)
		{
		case OP_ALLOCAI:
		{
			// TODO(eesuck): add bulk alloc or bulk insert zero/none into linked array implementation
			VM_Val none = { 0 };
			VM_Val size = ee_vm_prog_get_imm(prog, op.r_d);

			for (size_t i = 0; i < (VM_Word)size.as_u64; ++i)
			{
				ee_linked_array_push(&vm->stack, EE_RECAST_U8(none));
			}

			vm->sp += (VM_Word)size.as_u64;
		} break;
		case OP_MOV:
		{
			VM_Val src = ee_vm_stack_at(vm, op.r_1);
			ee_vm_stack_set(vm, op.r_d, src);
		} break;
		case OP_MOVI:
		{
			VM_Val src = ee_vm_prog_get_imm(prog, op.r_1);
			ee_vm_stack_set(vm, op.r_d, src);
		} break;

		case OP_ADD:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 + src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_ADDI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 + src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SUB:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 - src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SUBI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 - src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_MUL:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 * src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_MULI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 * src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_IDIV:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 / src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_IDIVI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 / src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_UDIV:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 / src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_UDIVI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 / src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_IMOD:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 % src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_IMODI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 % src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_UMOD:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 % src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_UMODI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 % src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_FADD:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 + src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FADDI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 + src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FSUB:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 - src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FSUBI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 - src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FMUL:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 * src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FMULI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 * src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FDIV:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 / src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FDIVI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f32 = src_1.as_f32 / src_0.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_DADD:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 + src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DADDI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 + src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DSUB:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 - src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DSUBI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 - src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DMUL:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 * src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DMULI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 * src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DDIV:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 / src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DDIVI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_f64 = src_1.as_f64 / src_0.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_NOT:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_u64 = ~src_1.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_OR:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 | src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_ORI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 | src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_AND:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 & src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_ANDI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 & src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_XOR:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 ^ src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_XORI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 ^ src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SHL:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 << src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SHLI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 << src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SHR:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 >> src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SHRI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 >> src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SAL:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 << src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SALI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 << src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SAR:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 >> src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SARI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 >> src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_JMP:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			vm->ip = (VM_Word)dest.as_u64;
		} break;
		case OP_JMPI:
		{
			VM_Val dest = ee_vm_prog_get_imm(prog, op.r_d);
			vm->ip = (VM_Word)dest.as_u64;
		} break;
		case OP_JIZ:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			VM_Val cond = ee_vm_stack_at(vm, op.r_1);

			if (cond.as_u64 == 0)
			{
				vm->ip = (VM_Word)dest.as_u64;
			}
		} break;
		case OP_JIZI:
		{
			VM_Val dest = ee_vm_prog_get_imm(prog, op.r_d);
			VM_Val cond = ee_vm_stack_at(vm, op.r_1);

			if (cond.as_u64 == 0)
			{
				vm->ip = (VM_Word)dest.as_u64;
			}
		} break;
		case OP_JNZ:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			VM_Val cond = ee_vm_stack_at(vm, op.r_1);

			if (cond.as_u64 != 0)
			{
				vm->ip = (VM_Word)dest.as_u64;
			}
		} break;
		case OP_JNZI:
		{
			VM_Val dest = ee_vm_prog_get_imm(prog, op.r_d);
			VM_Val cond = ee_vm_stack_at(vm, op.r_1);

			if (cond.as_u64 != 0)
			{
				vm->ip = (VM_Word)dest.as_u64;
			}
		} break;

		case OP_SEQ:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 == src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SEQI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 == src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SNEQ:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 != src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SNEQI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 != src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLEQ:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 <= src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLEQI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 <= src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGEQ:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 >= src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGEQI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 >= src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLT:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 < src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLTI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 < src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGT:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 > src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGTI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_i64 = src_1.as_i64 > src_0.as_i64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_SLEQU:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 <= src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLEQUI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 <= src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGEQU:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 >= src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGEQUI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 >= src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLTU:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 < src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SLTUI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 < src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGTU:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_stack_at(vm, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 > src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SGTUI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val src_0 = ee_vm_prog_get_imm(prog, op.r_0);
			VM_Val res = { .as_u64 = src_1.as_u64 > src_0.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_ITOF:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_f32 = (f32)src_1.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FTOI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_u64 = (u64)src_1.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_FTOD:
		{
			VM_Val src = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_f64 = (f64)src.as_f32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_DTOF:
		{
			VM_Val src = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_f32 = (f32)src.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_ITOD:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_f64 = (f64)src_1.as_u64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_DTOI:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_u64 = (u64)src_1.as_f64 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SEXT8:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_i64 = (i64)src_1.as_i8 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SEXT16:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_i64 = (i64)src_1.as_i16 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;
		case OP_SEXT32:
		{
			VM_Val src_1 = ee_vm_stack_at(vm, op.r_1);
			VM_Val res = { .as_i64 = (i64)src_1.as_i32 };
			ee_vm_stack_set(vm, op.r_d, res);
		} break;

		case OP_CALL:
		{
			VM_Val op_id = ee_vm_stack_at(vm, op.r_d);
			VM_Val op_argc = ee_vm_stack_at(vm, op.r_1);

			VM_Call_Frame frame = { 0 };

			frame.ip = vm->ip;
			frame.bp = vm->bp;

			vm->bp = vm->sp - (VM_Word)op_argc.as_u64;
			vm->ip = (VM_Word)op_id.as_u64;

			ee_linked_array_push(&vm->frames, EE_RECAST_U8(frame));
		} break;

		case OP_CALLI:
		{
			VM_Val op_id = ee_vm_prog_get_imm(prog, op.r_d);
			VM_Val op_argc = ee_vm_prog_get_imm(prog, op.r_1);

			VM_Call_Frame frame = { 0 };

			frame.ip = vm->ip;
			frame.bp = vm->bp;

			vm->bp = vm->sp - (VM_Word)op_argc.as_u64;
			vm->ip = (VM_Word)op_id.as_u64;

			ee_linked_array_push(&vm->frames, EE_RECAST_U8(frame));
		} break;
		case OP_RET:
		{
			VM_Call_Frame frame = { 0 };

			ee_linked_array_pop(&vm->frames, EE_RECAST_U8(frame));

			vm->sp = vm->bp;
			vm->ip = frame.ip;
			vm->bp = frame.bp;
		} break;

		case OP_LOAD64:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = { 0 };

			memcpy(&val, ptr.as_anyptr, 8);
			ee_vm_stack_set(vm, op.r_d, val);
		} break;
		case OP_LOAD32:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = { 0 };

			memcpy(&val, ptr.as_anyptr, 4);
			ee_vm_stack_set(vm, op.r_d, val);
		} break;
		case OP_LOAD16:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = { 0 };

			memcpy(&val, ptr.as_anyptr, 2);
			ee_vm_stack_set(vm, op.r_d, val);
		} break;
		case OP_LOAD8:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = { 0 };

			memcpy(&val, ptr.as_anyptr, 1);
			ee_vm_stack_set(vm, op.r_d, val);
		} break;

		case OP_STORE64:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_stack_at(vm, op.r_1);

			memcpy(ptr.as_anyptr, &val, 8);
		} break;
		case OP_STORE32:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_stack_at(vm, op.r_1);

			memcpy(ptr.as_anyptr, &val, 4);
		} break;
		case OP_STORE16:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_stack_at(vm, op.r_1);

			memcpy(ptr.as_anyptr, &val, 2);
		} break;
		case OP_STORE8:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_stack_at(vm, op.r_1);

			memcpy(ptr.as_anyptr, &val, 1);
		} break;

		case OP_STORE64I:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_1);

			memcpy(ptr.as_anyptr, &val, 8);
		} break;
		case OP_STORE32I:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_1);

			memcpy(ptr.as_anyptr, &val, 4);
		} break;
		case OP_STORE16I:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_1);

			memcpy(ptr.as_anyptr, &val, 2);
		} break;
		case OP_STORE8I:
		{
			VM_Val ptr = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_1);

			memcpy(ptr.as_anyptr, &val, 1);
		} break;

		case OP_MEMCPY:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			VM_Val src = ee_vm_stack_at(vm, op.r_1);
			VM_Val size = ee_vm_stack_at(vm, op.r_0);

			memcpy(dest.as_anyptr, src.as_anyptr, size.as_u64);
		} break;
		case OP_MEMCPYI:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			VM_Val src = ee_vm_stack_at(vm, op.r_1);
			VM_Val size = ee_vm_prog_get_imm(prog, op.r_0);

			memcpy(dest.as_anyptr, src.as_anyptr, size.as_u64);
		} break;

		case OP_MALLOC:
		{
			VM_Val size = ee_vm_stack_at(vm, op.r_1);
			VM_Val result = { 0 };

			result.as_anyptr = ee_linked_arena_alloc(&vm->heap, size.as_u64);

			ee_vm_stack_set(vm, op.r_d, result);
		} break;
		case OP_MALLOCI:
		{
			VM_Val size = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val result = { 0 };

			result.as_anyptr = ee_linked_arena_alloc(&vm->heap, size.as_u64);
			
			ee_vm_stack_set(vm, op.r_d, result);
		} break;

		case OP_MEMSET:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_stack_at(vm, op.r_1);
			VM_Val size = ee_vm_stack_at(vm, op.r_0);

			memset(dest.as_anyptr, val.as_i32, size.as_u64);
		} break;

		case OP_MEMSETI:
		{
			VM_Val dest = ee_vm_stack_at(vm, op.r_d);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val size = ee_vm_prog_get_imm(prog, op.r_0);

			memset(dest.as_anyptr, val.as_i32, size.as_u64);
		} break;

		case OP_REGADDR:
		{
			VM_Val dest = { .as_anyptr = ee_vm_stack_at_ptr(vm, op.r_1) };
			ee_vm_stack_set(vm, op.r_d, dest);
		} break;

		case OP_REGSET8:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_stack_at(vm, op.r_1);
			VM_Val val  = ee_vm_stack_at(vm, op.r_0);

			dest->as_bytes[index.as_u64] = val.as_u8;
		} break;
		case OP_REGSET16:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_words[index.as_u64] = val.as_u16;
		} break;
		case OP_REGSET32:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_dwords[index.as_u64] = val.as_u32;
		} break;

		case OP_REGSET8I:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val val  = ee_vm_prog_get_imm(prog, op.r_0);

			dest->as_bytes[index.as_u64] = val.as_u8;
		} break;
		case OP_REGSET16I:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_0);

			dest->as_words[index.as_u64] = val.as_u16;
		} break;
		case OP_REGSET32I:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val val = ee_vm_prog_get_imm(prog, op.r_0);

			dest->as_dwords[index.as_u64] = val.as_u32;
		} break;

		case OP_REGCPY8:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_bytes[index.as_u64] = val.as_bytes[index.as_u64];
		} break;
		case OP_REGCPY16:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_words[index.as_u64] = val.as_words[index.as_u64];
		} break;
		case OP_REGCPY32:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_stack_at(vm, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_dwords[index.as_u64] = val.as_dwords[index.as_u64];
		} break;
		case OP_REGCPY8I:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_bytes[index.as_u64] = val.as_bytes[index.as_u64];
		} break;
		case OP_REGCPY16I:
		{

			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_words[index.as_u64] = val.as_words[index.as_u64];
		} break;
		case OP_REGCPY32I:
		{
			VM_Val* dest = ee_vm_stack_at_ptr(vm, op.r_d);

			VM_Val index = ee_vm_prog_get_imm(prog, op.r_1);
			VM_Val val = ee_vm_stack_at(vm, op.r_0);

			dest->as_dwords[index.as_u64] = val.as_dwords[index.as_u64];
		} break;

		case OP_ASSERT:
		{
			EE_PRINTLN("\nASSERTATION IN VM AT INSTRUCTION (%d)", vm->ip);
			EE_PRINTLN("STATE START");
			ee_vm_debug_print(vm);
			EE_PRINTLN("\nSTATE END");

			vm->halt = EE_TRUE;
		} break;
		case OP_LOGSTATE:
		{
			EE_PRINTLN("\nVM STATE AT INSTRUCTION (%d)", vm->ip);
			EE_PRINTLN("STATE START");
			ee_vm_debug_print(vm);
			EE_PRINTLN("\nSTATE END");
		} break;

		case OP_HALT:
		{
			vm->halt = EE_TRUE;
		} break;
		default: EE_ASSERT(0, "Unknown op code (%d)", op.code); break;
		}
	}
}

void ee_vm_debug_print(const Virtual_Machine* vm)
{
	EE_PRINTLN("\nIP: (%d), SP: (%d), BP: (%d), TICKS: (%zu)", vm->ip, vm->sp, vm->bp, vm->ticks);
	EE_PRINTLN("STACK (%d):", vm->sp);
	EE_PRINT("    ");

	for (VM_Word i = 0; i < vm->sp; ++i)
	{
		VM_Val val = *(VM_Val*)ee_linked_array_at(&vm->stack, i);

		EE_PRINT("[0x%08X%08X]", val.as_dwords[1], val.as_dwords[0]);

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

	for (VM_Word i = 0; i < consts_count; ++i)
	{
		VM_Val val = *(VM_Val*)ee_linked_array_at(&prog->consts, i);

		EE_PRINT("[0x%08X%08X]", val.as_dwords[1], val.as_dwords[0]);

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
			EE_PRINT("%-10s", _s_op_names[op->code]);
		else
			EE_PRINT("OP_%-3d   ", op->code);

		if (op->r_0 != EE_VM_INVALID_REG)
		{
			EE_PRINTLN(" %c%-2u, %c%-2u, %c%-2u",
				(op->r_d & EE_VM_IMM_FLAG) ? 'i' : 'r', op->r_d & EE_VM_IMM_MASK,
				(op->r_1 & EE_VM_IMM_FLAG) ? 'i' : 'r', op->r_1 & EE_VM_IMM_MASK,
				(op->r_0 & EE_VM_IMM_FLAG) ? 'i' : 'r', op->r_0 & EE_VM_IMM_MASK);
		}
		else if (op->r_1 != EE_VM_INVALID_REG)
		{
			EE_PRINTLN(" %c%-2u, %c%-2u",
				(op->r_d & EE_VM_IMM_FLAG) ? 'i' : 'r', op->r_d & EE_VM_IMM_MASK,
				(op->r_1 & EE_VM_IMM_FLAG) ? 'i' : 'r', op->r_1 & EE_VM_IMM_MASK);
		}
		else if (op->r_d != EE_VM_INVALID_REG)
		{
			EE_PRINTLN(" %c%-2u",
				(op->r_d & EE_VM_IMM_FLAG) ? 'i' : 'r', op->r_d & EE_VM_IMM_MASK);
		}
		else
		{
			EE_PRINT("\n");
		}
	}
}

Semantic_Analyzer ee_sem_new(const Allocator* allocator)
{
	Semantic_Analyzer out = { 0 };

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

	out.ir = ee_vm_prog_new(EE_NKB(2), EE_NKB(2), EE_NMB(2), &out.allocator);

	return out;
}

void ee_sem_gen_expr(Semantic_Analyzer* sem, Ast_Expr* expr, Sem_Gen_Context context)
{
	switch (expr->type)
	{
	case EXPR_LIT:
	{
		VM_Val val = { 0 };

		switch (expr->as_lit.token->type)
		{
		case TOKEN_LIT_INT: val.as_u64 = expr->as_lit.token->as_u64;
		case TOKEN_LIT_FLOAT: val.as_f64 = expr->as_lit.token->as_f64;
		case TOKEN_LIT_STR: val.as_anyptr = expr->as_lit.token->as_str_view.buffer; // TODO(eesuck): figure this out
		case TOKEN_TRUE: val.as_u64 = EE_TRUE;
		case TOKEN_FALSE: val.as_u64 = EE_FALSE;
		}

		VM_Word cid = ee_vm_prog_push_const(&sem->ir, val);

		ee_vm_prog_push_op(&sem->ir, OP_MOVI, context.dest, cid, EE_VM_INVALID_REG);
	} break;
	case EXPR_IDENT:
	{
		EE_ASSERT(0, "To generate identifiers the symbols table need first, todo");
	} break;
	case EXPR_BINOP:
	{

	} break; 
	default:
	{
		EE_ASSERT(0, "Unknown expression type (%d)", expr->type);
	} break;
	}
}
