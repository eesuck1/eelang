#include "ee_log.h"

void ee_log(Log_Level lvl, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	ee_log_va(lvl, fmt, args);

	va_end(args);
}

void ee_log_va(Log_Level lvl, const char* fmt, va_list args)
{
	const char* header = NULL;

	switch (lvl)
	{
	case LOG_LVL_INFO: header = "[INFO]"; break;
	case LOG_LVL_WARNING: header = "[WARNING]"; break;
	case LOG_LVL_ERROR: header = "[ERROR]"; break;
	}

	fprintf(stderr, "%s ", header);
	vfprintf(stderr, fmt, args);
}

void ee_log_error_token(Logger* log, const Token* token, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	ee_log_error_token_va(log, token, fmt, args);

	va_end(args);
}

void ee_log_error_token_va(Logger* log, const Token* token, const char* fmt, va_list args)
{
	const char* buf = log->lexer->stream.buffer;

	i64 line_start = (i64)token->pos;
	i64 line_end   = (i64)token->pos;

	i64 line_start_p = line_start;
	i64 line_end_p   = line_end;

	i64 f_count = 0;
	i64 b_count = 0;

	while (line_start > 0 && buf[line_start] != '\n')
	{
		line_start--;
		
		if (!isspace(buf[line_start]))
		{
			line_start_p = line_start;
		}
	}

	while (line_end < (i64)ee_str_len(&log->lexer->stream) && buf[line_end] != '\n')
	{
		if (!isspace(buf[line_end]))
		{
			line_end_p = line_end;
		}

		line_end++;
	}

	i64 line_prev   = line_start - 1;
	i64 line_prev_p = line_prev;

	while (line_prev > 0 && buf[line_prev] != '\n')
	{
		line_prev--;

		if (!isspace(buf[line_prev]))
		{
			line_prev_p = line_prev;
		}
	}

	EE_ASSERT(line_end_p >= line_start_p, "Invalid bounbs for line (%lld, %lld)", line_start_p, line_end_p);

	size_t line_len = (size_t)(line_end_p - line_start_p) + 1;
	size_t prev_len = (size_t)(line_start_p - line_prev_p) - 1;

	fprintf(stderr, "\033[96m");

	ee_log(LOG_LVL_ERROR, "[%zu:%zu] ", token->line + 1, token->chr + 1);

	vfprintf(stderr, fmt, args);
	fputc('\n', stderr);

	if (prev_len > 0)
	{ 
		fprintf(stderr, " %6zu | ", token->line);
		fwrite(&buf[line_prev_p], 1, prev_len, stderr);
		fputc('\n', stderr);
	}

	fprintf(stderr, " %6zu | ", token->line + 1);
	fwrite(&buf[line_start_p], 1, line_len, stderr);
	fputc('\n', stderr);

	fprintf(stderr, "        | ");
	for (size_t i = line_start_p; i < (size_t)line_end_p; ++i)
	{
		if ((i >= token->pos) && (i < token->pos + token->scratch.len))
		{
			fputc('~', stderr);
		}
		else
		{
			fputc(' ', stderr);
		}
	}

	fputc('\n', stderr);

	fprintf(stderr, "\033[0m");
}
