/********************************************************************
 *
 * util.c
 *
 * Implements useful utility functions.
 *
 ********************************************************************/

#include <math.h>
#include "util.h"

/* Construct a point */
vec_t vec_make(double x, double y)
{
    vec_t v = { x, y };
    return v;
}

/* Add two points together */
vec_t vec_add(vec_t v1, vec_t v2)
{
    return vec_make(v1.x+v2.x, v1.y+v2.y);
}

/* Subtract two points */
vec_t vec_subtract(vec_t v1, vec_t v2)
{
    return vec_make(v1.x-v2.x, v1.y-v2.y);
}

/* Scale a point by a scalar */
vec_t vec_scale(double k, vec_t v)
{
    return vec_make(k*v.x, k*v.y);
}

/* Dot product of two points */
double vec_dot(vec_t v1, vec_t v2)
{
    return (v1.x*v2.x)+(v1.y*v2.y);
}

/* Squared norm of a vector */
double vec_sqnorm(vec_t v)
{
    return vec_dot(v, v);
}

/* Norm of a vector */
double vec_norm(vec_t v)
{
    return sqrt(vec_sqnorm(v));
}

/* Normalized copy of a vector */
vec_t vec_normalized(vec_t v)
{
    double norm = vec_norm(v);
    return vec_make(v.x/norm, v.y/norm);
}

/* Square the argument */
double square(double d)
{
    return d*d;
}

/* Compute the shortest distance between a point an a line
 *  a       First point on the line
 *  l1       Second point on the line
 *  l2       The point from which to measure the distance
 */
double line_dist(vec_t l1, vec_t l2, vec_t p)
{
    double sign = ((l2.x-l1.x)*(p.y-l1.y)-(l2.y-l1.y)*(p.x-l1.x)) > 0 ? 1 : -1;
    vec_t n = vec_normalized(vec_subtract(l2, l1));
    vec_t to_p = vec_subtract(l1, p);
    return sign * vec_norm(vec_subtract(to_p, vec_scale(vec_dot(to_p, n), n)));
}
