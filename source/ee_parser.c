#include "ee_parser.h"

Parser ee_pars_new(const Array* tokens, const Allocator* allocator)
{
	Parser out = { 0 };

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

	EE_ASSERT(out.allocator.alloc_fn != NULL, "Trying to set NULL alloc callback");
	EE_ASSERT(out.allocator.realloc_fn != NULL, "Trying to set NULL realloc callback");
	EE_ASSERT(out.allocator.free_fn != NULL, "Trying to set NULL free callback");

	out.tokens = tokens;
	out.nodes = ee_array_new(ee_array_len(tokens), sizeof(Ast_Node), allocator);
	out.pos = 0;

	return out;
}

Ast_Node_Desc ee_pars_emplace_node(Parser* pars, Ast_Type type, Ast_Node_Handle prev)
{
	Ast_Node_Desc out = { 0 };

	out.handle = ee_array_len(&pars->nodes);
	out.node_ptr = (Ast_Node*)ee_array_emplace(&pars->nodes);

	out.node_ptr->type = type;
	out.node_ptr->prev = prev;

	return out;
}

Ast_Node_Handle ee_pars_id_decl(Parser* pars, Ast_Node_Handle prev)
{
	Ast_Node_Desc id_node_desc = ee_pars_emplace_node(pars, NODE_ID_EXPR, prev);

	// TODO(eesuck): better error messages
	ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Invalid token in identifier expression");
	const Token* id_token = ee_pars_eat(pars);
	ee_pars_match_or_panic(pars, ':', "Invalid token in identifier expression expected ':'");
	id_node_desc.node_ptr->as_id.name = id_token->scratch;

	// TODO(eesuck): type information should be parsed elsewhere because it could be complicated
	// by now consider only simple types
	if (ee_pars_match(pars, '='))
	{
		// TODO(eesuck): parse expression here, by now only instant integer values
		const Token* val = ee_pars_eat(pars);

		Ast_Type_Info id_type = { 0 };
		id_type.dtype = DTYPE_U64;
		
		id_node_desc.node_ptr->as_id.type_info = id_type;

		Ast_Node_Desc val_node_desc = ee_pars_emplace_node(pars, NODE_LITERAL_EXPR, id_node_desc.handle);
		
		val_node_desc.node_ptr->as_lit.type_info = id_type;
		val_node_desc.node_ptr->as_lit.as_u64 = val->as_u64;

		id_node_desc.node_ptr->as_id.value = val_node_desc.handle;
	}
	else if (ee_pars_check_or_panic(pars, TOKEN_IDENTIFIER, "Invalid token in identifier expression"))
	{
		// TODO(eesuck): parse type information
		const Token* type_token = ee_pars_eat(pars);

		Ast_Type_Info id_type = { 0 };
		id_type.dtype = DTYPE_U64;

		id_node_desc.node_ptr->as_id.type_info = id_type;
		id_node_desc.node_ptr->as_id.value = EE_AST_NODE_NULL;

		if (ee_pars_match(pars, '='))
		{
			const Token* val = ee_pars_eat(pars);

			Ast_Node_Desc val_node_desc = ee_pars_emplace_node(pars, NODE_LITERAL_EXPR, id_node_desc.handle);

			val_node_desc.node_ptr->as_lit.type_info = id_type;
			val_node_desc.node_ptr->as_lit.as_u64 = val->as_u64;

			id_node_desc.node_ptr->as_id.value = val_node_desc.handle;
		}
	}

	ee_pars_match_or_panic(pars, ';', "Every statement should end with ';'");

	return id_node_desc.handle;
}

void ee_pars_run(Parser* pars)
{
	//const Token* current = ee_pars_eat(pars);

	ee_pars_id_decl(pars, 0);
	ee_pars_id_decl(pars, 0);
	ee_pars_id_decl(pars, 0);
}