/*
 * problem.c
 * Implementation of functions relating to the solver environment for the optimization problem.
 */

#include "problem.h"

/*
 */
double max(double x, double y) { return x > y ? x : y; }
double min(double x, double y) { return x > y ? y : x; }

/*
 */
void add_to_rect(double x, double y, double *x1, double *y1, double *x2, double *y2) 
{
    *x1 = min(x, *x1);
    *x2 = max(x, *x2);
    *y1 = min(y, *y1);
    *y2 = max(y, *y2);
}

/*
 * Return the coordinates of the extents in which the entire problem can be rendered.
 */
void get_render_extents(problem_state_t *problem_state, double *x1, double *y1, double *x2, double *y2)
{
    int i, j;
    problem_t *problem = problem_state->problem;

    *x1 = *x2 = problem_state->current_pos.x;
    *y1 = *y2 = problem_state->current_pos.y;

    add_to_rect(problem->start_pos.x, problem->start_pos.y, x1, y1, x2, y2);
    add_to_rect(problem->end_pos.x, problem->end_pos.y, x1, y1, x2, y2);
    for (i = 0; i < problem->polygon_count; i++) {
        for (j = 0; j < problem->polygons[i].point_count; j++) {
            point_t pt = problem->polygons[i].points[j];
            add_to_rect(pt.x, pt.y, x1, y1, x2, y2);
        }
    }
}
