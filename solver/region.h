/********************************************************************
 *
 * region.h 
 *
 * Declares the functions that are used to find a feasible trajectory
 * within a convex region.
 *
 ********************************************************************/

#ifndef _REGION_H_
#define _REGION_H_

#include "util.h"

/* Maximum number of polygons per region */
#define MAX_POLY 32

/* Maximum number of time steps */
#define MAX_STEPS 40

/*
 * Parameters specifying the convex region subproblem.
 */
typedef struct {
    double mass;
    double radius;
    vec_t p_0;            /* Initial position */
    vec_t v_0;            /* Initial velocity */
    vec_t d_0;            /* Initial direction */
    double omega_0;       /* Initial angular velocity */
    double l_0;           /* Initial left thruster force */
    double r_0;           /* Initial right thruster force */
    vec_t p_n;            /* Destination point */
    int N;                /* Number of time steps */
    double dt;            /* Length of time steps */

    vec_t poly[MAX_POLY]; /* Region polygon, specified in CW order */
    int polycount;        /* Number of vertices in polygon */

    double penalty_mu;    /* Scalar for penalty term in objective */
    double constraint_mu; /* Scalar for constraint term in objective */
} region_params_t;

/* Solver return values */
typedef enum { REGION_SOLVED, REGION_STUCK } region_result_t;

/* Try to compute a trajectory in the given region.
 *  l       Vector of length {params.N} that will receieve the left thruster
 *          values for each time step.
 *  r       Vector of length {params.N} that will receive the right thruster
 *          values for each time step.
 *  params  A structure containing the problem to solve.
 */
region_result_t compute_trajectory(double *l, double *r, region_params_t *params);

#endif

