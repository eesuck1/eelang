#ifndef EE_LOG_H
#define EE_LOG_H

#include "stdio.h"
#include "stdarg.h"

#include "ee_lexer.h"

#define EE_LOG_MAX_FORWARD     (2)
#define EE_LOG_MAX_BACKWARD    (2)

typedef enum Log_Level
{
	LOG_LVL_INFO    = 0,
	LOG_LVL_WARNING = 1,
	LOG_LVL_ERROR   = 2,
} Log_Level;

typedef struct Logger
{
	const Lexer* lexer;
} Logger;

void ee_log(Log_Level lvl, const char* fmt, ...);
void ee_log_va(Log_Level lvl, const char* fmt, va_list args);
void ee_log_error_token(Logger* log, const Token* token, const char* fmt, ...);
void ee_log_error_token_va(Logger* log, const Token* token, const char* fmt, va_list args);

#endif // EE_LOG_H