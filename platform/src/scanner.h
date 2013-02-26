/*
 * scanner.h
 * Interface to the token scanner for the optimization problem definition.
 */

#ifndef _SCANNER_H_
#define _SCANNER_H_

#include <setjmp.h>
#include <stdio.h>

#define MAX_IDENT_LEN 255
#define MAX_STRING_LEN 2048

/*
 * Scanner tokens
 */
typedef enum {
    T_NONE, T_IDENT, T_LBRACE, T_RBRACE, T_LPAREN, T_RPAREN,
    T_STRING, T_COLON, T_COMMA, T_NUMBER, T_EOF, T_EQ,
    T_SEMICOLON, T_UNKNOWN
} token_t;

/*
 * Representation of the inner state of the scanner.
 */
typedef struct scanner_t {
    FILE *f;
    int bufpos;
    int tokenline, tokenpos;
    int line, pos;
    char ident[MAX_IDENT_LEN+1];
    char str[MAX_STRING_LEN+1];
    char ch;
    token_t token;
    double val;
    jmp_buf jb;
    int eof;
    
    void (*errmsg_func)(struct scanner_t *);
} scanner_t;

int strceq(const char *s1, const char *s2);
double expect(scanner_t *sc, token_t token);
char *expect_string(scanner_t *sc);
int expect_bool(scanner_t *sc);
void fail(scanner_t *sc, const char *msg, ...);

const char *token_name(token_t token);
void scan_next_char(scanner_t *sc);
int scan_unsigned_number_literal(scanner_t *sc);
int scan_number_literal(scanner_t *sc);
void scan_dec_literal(scanner_t *sc);
void scan_string_literal(scanner_t *sc);
void scan_ident(scanner_t *sc);
void scan_next_token(scanner_t *sc);

#endif

