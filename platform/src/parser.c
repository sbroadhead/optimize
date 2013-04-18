/*
 * parser.c
 * Implementation of the optimization problem definition parser.
 */

#include <stdlib.h>
#include <string.h>
#include "scanner.h"
#include "parser.h"

/*
 */
static void errmsg(scanner_t *sc) {
    printf("Parse failure (line %d, col %d): ", sc->tokenline, sc->tokenpos);
}

/*
 * Parse a point of the form (x, y)
 */
point_t parse_point(scanner_t *sc)
{
    double x, y;
    point_t pt;

    expect(sc, T_LPAREN);
    x = expect(sc, T_NUMBER);
    expect(sc, T_COMMA);
    y = expect(sc, T_NUMBER);
    expect(sc, T_RPAREN);

    pt.x = x;
    pt.y = y;
    return pt;
}

/*
 * Parse the problem definition file for an instance of the iceburg optimization problem.
 */
int parse_problem_file(FILE *file, problem_t *problem)
{
    int failed = 0;
    char name[MAX_IDENT_LEN];
    polygon_t *poly;
    scanner_t sc;
    point_t zero_point = { 0, 0 };

    problem->start_pos = zero_point;
    problem->start_angle = 0.0;
    problem->end_pos = zero_point;
    problem->end_angle = 0.0;
    problem->polygon_count = 0;

    memset(&sc, 0, sizeof(scanner_t));
    sc.f = file;
    sc.line = 1;
    sc.errmsg_func = errmsg;

    if (setjmp(sc.jb)) {
        /* parser failed */
        failed = 1;
        goto out;
    }

    scan_next_token(&sc);
    while (sc.token != T_EOF) {
        if (sc.token != T_IDENT) {
            fail(&sc, "Expected %s but found %s", token_name(T_IDENT),
                    token_name(sc.token));
        }
        strcpy(name, sc.ident);
        scan_next_token(&sc);
        expect(&sc, T_EQ);
        if (strcmp(name, "start-pos") == 0) {
            problem->start_pos = parse_point(&sc);
        } else if (strcmp(name, "start-angle") == 0) {
            problem->start_angle = expect(&sc, T_NUMBER);
        } else if (strcmp(name, "end-pos") == 0) {
            problem->end_pos = parse_point(&sc);
        } else if (strcmp(name, "end-angle") == 0) {
            problem->end_angle = expect(&sc, T_NUMBER);
        } else if (strcmp(name, "radius") == 0) {
            problem->radius = expect(&sc, T_NUMBER);
        } else if (strcmp(name, "polygons") == 0) {
            poly = problem->polygons;

            expect(&sc, T_LBRACE);
            /* Parse a list of polygons */
            while (sc.token != T_RBRACE) {
                poly->id = (int)expect(&sc, T_NUMBER);

                expect(&sc, T_LBRACE);
                /* Parse a list of points */
                while (sc.token != T_RBRACE) {
                    poly->points[poly->point_count] = parse_point(&sc);
                    poly->point_count++;
                    if (sc.token == T_COMMA) {
                        scan_next_token(&sc);
                    } else if (sc.token != T_RBRACE) {
                        fail(&sc, "Expected %s or %s but found %s", token_name(T_COMMA),
                                token_name(T_RBRACE), token_name(sc.token));
                    }
                }
                expect(&sc, T_RBRACE);

                poly++;
                problem->polygon_count++;
                if (sc.token == T_COMMA) {
                    scan_next_token(&sc);
                } else if (sc.token != T_RBRACE) {
                    fail(&sc, "Expected %s or %s but found %s", token_name(T_COMMA),
                            token_name(T_RBRACE), token_name(sc.token));
                }
            }
            expect(&sc, T_RBRACE);
        } else {
            fail(&sc, "Unknown option name: %s", name);
        }
        expect(&sc, T_SEMICOLON);
    }

out:
    return failed;
}
