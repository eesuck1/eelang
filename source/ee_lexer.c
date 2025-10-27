#include "ee_lexer.h"

u64 ee_lex_parse_int(Str_View raw)
{
	i64 out = 0;
	i64 sign = 1;
	size_t i = 0;

	switch (raw.buffer[0])
	{
	case '-': i++; sign = -1; break;
	case '+': i++; break;
	}

	for (; i < raw.len; ++i)
	{
		out = 10 * out + (i64)(raw.buffer[i] - '0');
	}

	if (out < 0)
	{
		out = 0;
	}

	return out * sign;
}

f64 ee_lex_parse_float(Str_View raw)
{
	char temp_buffer[1024];

	memcpy(temp_buffer, raw.buffer, raw.len);
	temp_buffer[raw.len] = '\0';

	return atof(temp_buffer);
}

void ee_lex_debug_print_lit_val(const Token* token)
{
	switch (token->type)
	{
	case TOKEN_LIT_INT:
	{
		EE_PRINT("%llu", token->as_u64);
	} break;
	case TOKEN_LIT_FLOAT:
	{
		EE_PRINT("%f", token->as_f64);
	} break;
	case TOKEN_LIT_STR:
	{
		EE_ASSERT(0, "TODO string literal print");
	} break;
	default:
	{
		EE_ASSERT(0, "Invalid token type (%d) for debug print", token->type);
	}
	}
}

Lexer ee_lex_new_file(const char* file_path, Allocator* allocator)
{
	Lexer out = { 0 };

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

	out.stream = ee_str_from_file(file_path, EE_STR_FILE_READ_BYTES, &out.allocator);
	out.tokens = ee_array_new(EE_MAX(out.stream.top / 3, 32), sizeof(Token), &out.allocator);

	out.pos = 0;
	out.file_path = file_path;

	return out;
}

i32 ee_lex_skip_whitespace(Lexer* lex)
{
	i32 skipped = EE_FALSE;

	while (!ee_lex_end(lex))
	{
		if (isspace(ee_lex_peek(lex)))
		{
			ee_lex_advance(lex, 1);
			skipped = EE_TRUE;

			continue;
		}
		else if (ee_lex_peek(lex) == '/' && ee_lex_peek_next(lex, 1) == '/')
		{
			ee_lex_advance(lex, 2);
			skipped = EE_TRUE;

			continue;
		}

		break;
	}

	return skipped;
}

i32 ee_lex_process_id(Lexer* lex)
{
	if (ee_is_ident_start(ee_lex_peek(lex)))
	{
		Token_Type type = -1;
		size_t start_pos = lex->pos;
		
		ee_lex_advance(lex, 1);

		while (ee_is_ident_continue(ee_lex_peek(lex)))
			ee_lex_advance(lex, 1);

		size_t len = lex->pos - start_pos;
		Str_View str = ee_str_view_from_str(&lex->stream, start_pos, len);

		switch (len)
		{
		case 2:
		{
			if (memcmp(str.buffer, EE_LEX_FN_STR, sizeof(EE_LEX_FN_STR) - 1) == 0)
				type = TOKEN_FUNCTION;
		} break;
		default: break;
		}

		if (type == -1)
			type = TOKEN_IDENTIFIER;

		ee_lex_emit_token(lex, type, str);

		return EE_TRUE;
	}

	return EE_FALSE;
}

i32 ee_lex_process_literal(Lexer* lex)
{
	size_t start_pos = lex->pos;

	if (ee_is_digit(ee_lex_peek(lex)))
	{
		ee_lex_advance(lex, 1);

		while (ee_is_digit(ee_lex_peek(lex)))
			ee_lex_advance(lex, 1);

		if (ee_lex_check(lex, '.'))
		{
			ee_lex_advance(lex, 1);

			while (ee_is_digit(ee_lex_peek(lex)))
				ee_lex_advance(lex, 1);

			Str_View f_str = ee_str_view_from_str(&lex->stream, start_pos, lex->pos - start_pos);

			ee_lex_emit_token_f64(lex, f_str, ee_lex_parse_float(f_str));

			return EE_TRUE;
		}

		Str_View str = ee_str_view_from_str(&lex->stream, start_pos, lex->pos - start_pos);

		ee_lex_emit_token_u64(lex, str, ee_lex_parse_int(str));

		return EE_TRUE;
	}
	else if (ee_lex_check(lex, '.') && ee_is_digit(ee_lex_peek_next(lex, 1)))
	{
		ee_lex_advance(lex, 1);

		while (ee_is_digit(ee_lex_peek(lex)))
			ee_lex_advance(lex, 1);

		Str_View f_str = ee_str_view_from_str(&lex->stream, start_pos, lex->pos - start_pos);

		ee_lex_emit_token_f64(lex, f_str, ee_lex_parse_float(f_str));

		return EE_TRUE;
	}
	else if (ee_lex_check(lex, '"'))
	{
		// TODO(eesuck): string literals
		EE_ASSERT(0, "string literals not done yet");
	}

	return EE_FALSE;
}

void ee_lex_emit_after_equal(Lexer* lex, Token_Type token, Token_Type if_equal)
{
	if (ee_lex_peek_next(lex, 1) == '=')
	{
		Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 2);

		ee_lex_emit_token(lex, if_equal, scratch);
		ee_lex_advance(lex, 2);
	}
	else
	{
		Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 1);

		ee_lex_emit_token(lex, token, scratch);
		ee_lex_advance(lex, 1);
	}
}

void ee_lex_emit_token(Lexer* lex, Token_Type token_type, Str_View scratch)
{
	Token token = { 0 };

	token.type = token_type;
	token.scratch = scratch;

	ee_array_push(&lex->tokens, EE_RECAST_U8(token));
}

void ee_lex_emit_token_u64(Lexer* lex, Str_View scratch, u64 val)
{
	Token token = { 0 };

	token.type = TOKEN_LIT_INT;
	token.scratch = scratch;
	token.as_u64 = val;

	ee_array_push(&lex->tokens, EE_RECAST_U8(token));
}

void ee_lex_emit_token_f64(Lexer* lex, Str_View scratch, f64 val)
{
	Token token = { 0 };

	token.type = TOKEN_LIT_FLOAT;
	token.scratch = scratch;
	token.as_f64 = val;

	ee_array_push(&lex->tokens, EE_RECAST_U8(token));
}

void ee_lex_emit_token_str_view(Lexer* lex, Token_Type token_type, Str_View scratch, Str_View val)
{
	Token token = { 0 };

	token.type = token_type;
	token.scratch = scratch;
	token.as_str_view = val;

	ee_array_push(&lex->tokens, EE_RECAST_U8(token));
}

void ee_lex_tokenize(Lexer* lex)
{
	while (!ee_lex_end(lex))
	{
		if (ee_lex_skip_whitespace(lex))
			continue;

		if (ee_lex_end(lex))
			break;

		if (ee_lex_process_id(lex))
			continue;

		if (ee_lex_process_literal(lex))
			continue;

		char current = ee_lex_peek(lex);
		
		switch (current)
		{
		case '(':
		case ')':
		case '{':
		case '}':
		case '[':
		case ']':
		case ';':
		case ':':
		case ',':
		case '.':
		{
			Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 1);

			ee_lex_emit_token(lex, current, scratch);
			ee_lex_advance(lex, 1);
		} break;
		case '+':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_PLUS_EQUAL);
		} break;
		case '-':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_MINUS_EQUAL);
		} break;
		case '*':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_MUL_EQUAL);
		} break;
		case '/':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_DIV_EQUAL);
		} break;
		case '=':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_EQUAL_EQUAL);
		} break;
		case '!':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_NOT_EQUAL);
		} break;
		case '^':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_BW_XOR_EQUAL);
		} break;
		case '~':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_BW_NOT_EQUAL);
		} break;
		case '<':
		{
			if (ee_lex_peek_next(lex, 1) == '<')
			{
				Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 2);

				ee_lex_emit_token(lex, TOKEN_SHIFT_LEFT, scratch);
				ee_lex_advance(lex, 2);

				break;
			}

			ee_lex_emit_after_equal(lex, current, TOKEN_LESS_EQUAL);
		} break;
		case '>':
		{
			if (ee_lex_peek_next(lex, 1) == '>')
			{
				Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 2);

				ee_lex_emit_token(lex, TOKEN_SHIFT_RIGHT, scratch);
				ee_lex_advance(lex, 2);

				break;
			}

			ee_lex_emit_after_equal(lex, current, TOKEN_GREATER_EQUAL);
		} break;
		case '%':
		{
			ee_lex_emit_after_equal(lex, current, TOKEN_MOD_EQUAL);
		} break;
		case '&':
		{
			if (ee_lex_peek_next(lex, 1) == '&' && ee_lex_peek_next(lex, 2) == '=')
			{
				Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 3);

				ee_lex_emit_token(lex, TOKEN_AND_EQUAL, scratch);
				ee_lex_advance(lex, 3);
			}
			else if (ee_lex_peek_next(lex, 1) == '&')
			{
				Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 2);

				ee_lex_emit_token(lex, TOKEN_AND, scratch);
				ee_lex_advance(lex, 2);
			}
			else
			{
				ee_lex_emit_after_equal(lex, current, TOKEN_BW_XOR_EQUAL);
			}
		} break;
		case '|':
		{
			if (ee_lex_peek_next(lex, 1) == '|' && ee_lex_peek_next(lex, 2) == '=')
			{
				Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 3);

				ee_lex_emit_token(lex, TOKEN_OR_EQUAL, scratch);
				ee_lex_advance(lex, 3);
			}
			else if (ee_lex_peek_next(lex, 1) == '|')
			{
				Str_View scratch = ee_str_view_from_str(&lex->stream, lex->pos, 2);

				ee_lex_emit_token(lex, TOKEN_OR, scratch);
				ee_lex_advance(lex, 2);
			}
			else
			{
				ee_lex_emit_after_equal(lex, current, TOKEN_BW_XOR_EQUAL);
			}
		} break;

		default:
		{
			EE_ASSERT(0, "Unknown character (%c) at position (%zu)", ee_lex_peek(lex), lex->pos);
		}
		}

	}

	Str_View eof_str = ee_str_view_new("", 0);
	
	ee_lex_emit_token(lex, TOKEN_EOF, eof_str);
}
