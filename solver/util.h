/********************************************************************
 *
 * util.h
 *
 * Declares helpful utility functions.
 *
 ********************************************************************/

#ifndef _UTIL_H_
#define _UTIL_H_

/*
 * 2-dimensional point or vector
 */
typedef struct {
    double x;
    double y;
} vec_t;

/* Construct a point */
vec_t vec_make(double x, double y);

/* Add two points together */
vec_t vec_add(vec_t v1, vec_t v2);

/* Subtract two points */
vec_t vec_subtract(vec_t v1, vec_t v2);

/* Scale a point by a scalar */
vec_t vec_scale(double k, vec_t v);

/* Dot product of two points */
double vec_dot(vec_t v1, vec_t v2);

/* Squared norm of a vector */
double vec_sqnorm(vec_t v);

/* Norm of a vector */
double vec_norm(vec_t v);

/* Normalized copy of a vector */
vec_t vec_normalized(vec_t v);

/* Square the argument */
double square(double d);

/* Compute the shortest distance between a point an a line
 *  l1      First point on the line
 *  l2      Second point on the line
 *  p       The point from which to measure the distance
 */
double line_dist(vec_t l1, vec_t l2, vec_t p);

#endif

