/*
 * problem.h
 * Interface to the optimization problem.
 */

#ifndef _PROBLEM_H_
#define _PROBLEM_H_

#define MAX_POLYGONS 128
#define MAX_POLYGON_POINTS 16

/*
 */
typedef struct {
    double x, y;
} point_t;

/*
 */
typedef struct {
    int id;
    point_t points[MAX_POLYGON_POINTS];
    int point_count;
} polygon_t;

/*
 * Representation of the iceburg optimization problem
 */
typedef struct {
    point_t start_pos;
    double start_angle;
    point_t end_pos;
    double end_angle;
    polygon_t polygons[MAX_POLYGONS];
    int polygon_count;
} problem_t;

/*
 * Representation of the current problem state
 */
typedef struct {
    int tick;
    point_t current_pos;
    double current_angle;
    problem_t *problem;

    double x1, y1, x2, y2; /* view extents */
} problem_state_t;

void get_render_extents(problem_state_t *problem_state, double *x1, double *y1, double *x2, double *y2);

#endif

