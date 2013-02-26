/*
 * scanner.c
 * Implementation of the scanner for the optimization problem definition scanner.
 */

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <setjmp.h>
#include "problem.h"
#include "scanner.h"

/*
 * Case insensitive string equality
 */
int strceq(const char *s1, const char *s2) {
    while (*s1 || *s2) {
        if (toupper(*s1++) != toupper(*s2++)) return 0;
    }
    return 1;
}

/*
 * Abort the parser and output a failure message. This longjmps back to the
 * location where the scanner was initialized and fails from there.
 */
void fail(scanner_t *sc, const char *msg, ...)
{
    va_list args;
    va_start(args, msg);
    sc->errmsg_func(sc);
    vprintf(msg, args);
    printf("\n");
    va_end(args);

    longjmp(sc->jb, 1);
}

/*
 * Skip over a token, but fail if it does not occur. Returns the value of
 * the token, if applicable
 */
double expect(scanner_t *sc, token_t token)
{
    double val;

    if (sc->token != token) {
        fail(sc, "Expected %s but found %s (last value = %f, last ident = %s)",
                token_name(token), token_name(sc->token), sc->val, sc->ident);
    }
    val = sc->val;
    scan_next_token(sc);
    return val;
}

/*
 * Skip over a token, but fail if it is not a string. Returns a pointer to the
 * string, which must be freed by the caller
 */
char *expect_string(scanner_t *sc)
{
    char *str;
    int len;
        
    if (sc->token != T_STRING) {
        fail(sc, "Expected string\n");
    }
    len = strlen(sc->str);
    str = malloc(len + 1);
    strncpy(str, sc->str, len);
    str[len] = 0;
    scan_next_token(sc);
    
    return str;
}

/*
 * Skip over a token, but fail if it is not either 'true' or 'false'. Returns
 * the integer value of the boolean.
 */
int expect_bool(scanner_t *sc)
{
    if (sc->token != T_IDENT || !(strceq(sc->ident, "true") ||
            strceq(sc->ident, "false"))) {
        fail(sc, "Expected boolean but found %s", token_name(sc->token));
    }
    
    return strceq(sc->ident, "true");
}

/*
 * Get the name of a token.
 */
const char *token_name(token_t token)
{
    switch (token) {
        case T_IDENT: return "identifier";
        case T_COMMA: return ",";
        case T_COLON: return ":";
        case T_LPAREN: return "(";
        case T_RPAREN: return ")";
        case T_LBRACE: return "{";
        case T_RBRACE: return "}";
        case T_EQ: return "=";
        case T_STRING: return "string";
        case T_NUMBER: return "number";
        case T_EOF: return "end-of-input";
        case T_SEMICOLON: return ";";
        case T_NONE: return "<none>";
        default: return "unknown token";
    }
}

/*
 * Advance the scanner to the next character.
 */
void scan_next_char(scanner_t *sc)
{
    sc->pos++;
    if (sc->ch == '\n') {
        sc->pos = 1;
        sc->line++;
    }
    
    sc->ch = fgetc(sc->f);
    if (feof(sc->f)) {
        sc->eof = 1;
    }
}

/*
 * Read an unsigned number literal from the scanner and return whether it is a double or an integer.
 */
int scan_unsigned_number_literal(scanner_t *sc)
{
    int is_double = 0;
    double val = sc->val;
    scan_dec_literal(sc);
    if (sc->ch == '.') {
        scan_next_char(sc);
        is_double = 1;
        scan_dec_literal(sc);
        while (sc->val > 1) {
            sc->val /= 10;
        }
        sc->val += val;
    }
    return is_double;
}

/*
 * Read any signed number literal from the scanner and return whether it is a double or an integer.
 */
int scan_number_literal(scanner_t *sc)
{
    int r;
    if (sc->ch == '-') {
        scan_next_char(sc);
        if (!(sc->ch >= '0' && sc->ch <= '9')) {
            fail(sc, "Expected %s", token_name(T_NUMBER));
        }
        r = scan_unsigned_number_literal(sc);
        sc->val = -sc->val;
        return r;
    } else {
        return scan_unsigned_number_literal(sc);
    }
}

/*
 * Read a decimal literal from the scanner.
 */
void scan_dec_literal(scanner_t *sc)
{
    sc->val = 0;
    while (sc->ch >= '0' && sc->ch <= '9') {
        sc->val = (sc->val * 10) + (sc->ch - '0');
        scan_next_char(sc);
    }
}

/*
 * Read a string literal from the scanner.
 */
void scan_string_literal(scanner_t *sc)
{
    int i = 0;

    scan_next_char(sc);
    while (sc->ch != '"') {
        if (sc->ch == '\\') {
            /* handle escape characters */
            scan_next_char(sc);
            switch (sc->ch) {
                case 'n':  sc->str[i] = '\n'; break;
                case 'r':  sc->str[i] = '\r'; break;
                case 't':  sc->str[i] = '\t'; break;
                default:   sc->str[i] = sc->ch; break;
            }
        } else if (sc->ch == '\n') {
            fail(sc, "Line break in string literal");
        } else {
            sc->str[i] = sc->ch;
        }

        scan_next_char(sc);
        i++;

        if (i >= MAX_STRING_LEN) {
            fail(sc, "String literal too long");
        }

    }

    scan_next_char(sc);
    sc->str[i] = 0;
}

/*
 * Read an identifier from the scanner.
 */
void scan_ident(scanner_t *sc)
{
    int i = 0;
    while ((sc->ch >= 'A' && sc->ch <= 'Z') || 
            (sc->ch >= 'a' && sc->ch <= 'z') ||
            (sc->ch >= '0' && sc->ch <= '9') ||
            sc->ch == '_' || sc->ch == '.' || sc->ch == '+' || sc->ch == '@' || sc->ch == '-') {
        sc->ident[i] = sc->ch;
        i++;
        if (i >= MAX_IDENT_LEN) {
            fail(sc, "Identifier too long");
        }
        scan_next_char(sc);
    }
    sc->ident[i] = 0;
}

/*
 * Advance the scanner to the next token.
 */
void scan_next_token(scanner_t *sc)
{
    if (sc->token == T_NONE) {
        scan_next_char(sc);
    }

    /* skip whitespace */
    while (sc->ch <= ' ' && !sc->eof) {
        scan_next_char(sc);
    }
    
    /* skip comments */
    while (sc->ch == '#') {
        while (sc->ch != '\n' && !sc->eof) {
            scan_next_char(sc);
        }
        scan_next_char(sc);
    }

    sc->tokenline = sc->line;
    sc->tokenpos = sc->pos;
    
    if (sc->eof) {
        sc->token = T_EOF;
        return;
    }

    switch (sc->ch) {
        case ':': sc->token = T_COLON; scan_next_char(sc); break;
        case '=': sc->token = T_EQ; scan_next_char(sc); break;
        case ',': sc->token = T_COMMA; scan_next_char(sc); break;
        case '(': sc->token = T_LPAREN; scan_next_char(sc); break;
        case ')': sc->token = T_RPAREN; scan_next_char(sc); break;
        case '{': sc->token = T_LBRACE; scan_next_char(sc); break;
        case '}': sc->token = T_RBRACE; scan_next_char(sc); break;
        case ';': sc->token = T_SEMICOLON; scan_next_char(sc); break;
        case '"': scan_string_literal(sc); sc->token = T_STRING; break;
        case '1': case '2': case '3': case '4': case '5': case '6':
        case '7': case '8': case '9': case '0': case '-':
            scan_number_literal(sc);
            sc->token = T_NUMBER;
            break;
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
        case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
        case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
        case 's': case 't': case 'u': case 'v': case 'w': case 'x':
        case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
        case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
        case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
        case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
        case 'Y': case 'Z': case '_': case '.': case '+': case '@':
            sc->token = T_IDENT;
            scan_ident(sc);
            break;
        default:
            sc->token = T_UNKNOWN;
            scan_next_char(sc);
            break;
    }
}

