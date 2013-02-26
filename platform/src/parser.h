/*
 * parser.h
 * Interface to the problem definition parser
 */

#ifndef _PARSER_H_
#define _PARSER_H_

#include "problem.h"
#include "scanner.h"

int parse_problem_file(FILE *file, problem_t *problem);
point_t parse_point(scanner_t *sc);

#endif

