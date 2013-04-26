/********************************************************************
 *
 * region.c 
 *
 * The main solver code for finding a feasible trajectory within a
 * convex polygon.
 *
 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lbfgs.h>
#include <math.h>
#include "region.h"

#define USE_MACLAURIN 1

/* Compute the derivative with respect to the {var}th variable in the 
 * input vector.
 *  func    The function to differentiate
 *  x       The vector at which to evaluate the derivative
 *  params  The parameters with which to evaluate the function
 *  n       The number of variables
 *  var     The variable with which to take the derivative
 */
static double diff(double (*func)(const double *, const region_params_t *),
        const double *x, const region_params_t *params, int n, int var)
{
    const double h = 1e-6; /* Step size */
    const double hinv = 1.0/h;
    double *xx = (double *)malloc(sizeof(x[0]) * n);
    double f1, f2;

    memcpy(xx, x, sizeof(x[0])*n);
    xx[var] = x[var]+h;
    f1 = func(xx, params);
    xx[var] = x[var]-h;
    f2 = func(xx, params);
    free(xx);

    return 0.5 * hinv * (f1 - f2);
}

/* Evaluate the Maclaurin series of the direction differential equation
 * to a given order. This also computes the integral of the direction
 * series over the time interval to compute the Maclaurin series for the
 * velocity, and integrates that over the time interval to compute the
 * Maclaurin series for the position.
 *
 *  t       The time at which to evaluate the polynomial
 *  n       The number of terms to evaluate
 *  d       The initial direction
 *  v       The initial velocity
 *  p       The initial position
 *  l       The left thrust
 *  r       The right thrust
 *  omega   The initial angular velocity
 *  mass    The mass of the object
 *  radius  The radius of the object
 *  d_out   The variable to receive the final direction
 *  v_out   The variable to receive the final velocity
 *  p_out   The variable to receive the final position
 */
static void maclaurin(double t, int n, vec_t d, vec_t v, vec_t p, double l,
        double r, double omega, double mass, double radius, vec_t *d_out,
        vec_t *v_out, vec_t *p_out)
{
    static double a[30], b[30]; /* max 30 terms */
    vec_t dd, dv, dp;
    int i;

    a[0] = d.x;         b[0] = d.y;
    a[1] = -b[0]*omega; b[1] = a[0]*omega;
    for (i = 2; i < n; i++) {
        a[i] = (-b[i-1] / i * omega - b[i-2] / (i-1) * 2*(l-r)/(mass * radius));
        b[i] = ( a[i-1] / i * omega + a[i-2] / (i-1) * 2*(l-r)/(mass * radius));
    }

    dd = vec_make(0, 0);
    dv = vec_make(0, 0);
    dp = vec_make(0, 0);
    for (i = n-1; i >= 0; i--) {
        /* Add term to direction */
        dd.x = dd.x * t + a[i];
        dd.y = dd.y * t + b[i];

        /* Add term to velocity */
        dv.x = dv.x * t + a[i] / (i+1);
        dv.y = dv.y * t + b[i] / (i+1);

        /* Add term to position */
        dp.x = dp.x * t + a[i] / ((i+1)*(i+2));
        dp.y = dp.y * t + b[i] / ((i+1)*(i+2));
    }

    /* Correct velocity */
    dv = vec_scale(t * (l+r)/mass, dv);
    /* Correct position */
    dp = vec_scale(t*t, dp);

    *d_out = dd;
    *v_out = vec_add(v, dv);
    *p_out = vec_add(vec_add(vec_scale(t, v), p), dp);
}


/* Inverse barrier function to enforce the relation
 *     x <= y
 * in an objective function with the scalar mu.
 */
static double ib(double mu, double x, double y) 
{
    return mu * ((y <= x) ? INFINITY : 1/(y - x));
}

/* Quadratic penalty function to discourage the relation
 *     x <= y
 * from being violated, with the scalar mu.
 */
static double qp(double mu, double x, double y)
{
    return mu * ((y <= x) ? (y-x)*(y-x) : 0);
}

/* The objective function for the optimization problem
 *  x       The vector at which to evaluate the objective function.
 */
static double objective(const double *x, const region_params_t *params)
{
    int cur = 1, prev = 0;
    vec_t p[2], v[2], d[2];
    vec_t a;
    double omega[2];
    double domega;
    double theta;
    double y = 0.0;
    double cons = 0.0;      /* Constraint terms */
    double penalty = 0.0;   /* Penalty terms */
    int ccount;         /* Number of constraints */
    int pcount;         /* Number of penalties */
    int i, j, k;
    const double *l, *r;

    const int iters = 5;
    const double h = params->dt / iters;

    p[prev] = params->p_0;
    v[prev] = params->v_0;
    d[prev] = params->d_0;
    omega[prev] = params->omega_0;

    l = x;
    r = x+params->N;

    pcount = 0;
    ccount = 0;

#define CONSTRAINT(x, y) do{double c_=ib(params->constraint_mu,x,y)/(ccount+1);\
    double cc_=(ccount?(cons/(1.0+1.0/ccount)):0);\
    cons=c_+cc_;ccount++;}while(0)
#define PENALTY(x, y) do{double p_=qp(params->penalty_mu,x,y)/(pcount+1);\
    double pc_=(pcount?(penalty/(1.0+1.0/pcount)):0);\
    penalty=p_+pc_;pcount++;}while(0)

    for (i = 1; i <= params->N; i++) {
        for (j = 0; j < iters; j++) {
            /* Enforce unit direction */
            d[prev] = vec_normalized(d[prev]);

            /* Integrate the angular acceleration over the time step to
             * get angular velocity */
            domega = 2*(l[i-1] - r[i-1])/(params->radius * params->mass);
            omega[cur] = omega[prev] + h * domega;

#if USE_MACLAURIN
            /* Evaluate our Maclaurin series approximation to the movement of
             * our object */
            maclaurin(h, 7, d[prev], v[prev], p[prev], l[i-1], r[i-1],
                omega[prev], params->mass, params->radius, &d[cur], &v[cur],
                &p[cur]);
#else
            /* direction */
            theta = 0.5 * h * (omega[cur] + omega[prev]);
            d[cur].x = d[prev].x * cos(theta) - d[prev].y * sin(theta);
            d[cur].y = d[prev].x * sin(theta) + d[prev].y * cos(theta);

            /* acceleration */
            a = vec_scale((l[i-1] + r[i-1]) / params->mass, d[prev]);
            /* velocity */
            v[cur] = vec_add(v[prev], vec_scale(h, a));
            /* position */
            p[cur] = vec_add(p[prev], vec_add(vec_scale(h*h*0.5, a), vec_scale(h, v[prev])));
#endif

            /* Minimize the distance from the goal at the start of each time step */
            y += vec_sqnorm(vec_subtract(p[cur], params->p_n));
            //y += square(p[cur].x - params->p_n.x);
            //y += square(p[cur].y - params->p_n.y);

            /* Feasible region constraints */
            for (k = 0; k < params->polycount; k++) {
                double dist = line_dist(params->poly[k],
                    params->poly[(k+1)%params->polycount], p[cur]);
                PENALTY(params->radius, dist);
            }

            prev = cur;
            cur = 1 - cur;
        }
    }

    CONSTRAINT(square(l[0] - params->l_0), square(params->dt));
    CONSTRAINT(square(r[0] - params->r_0), square(params->dt));

    PENALTY(vec_sqnorm(v[prev]), 0);
    PENALTY(vec_sqnorm(vec_subtract(p[prev], params->p_n)), 0);

    for (i = 1; i < params->N; i++) {
        CONSTRAINT(square(l[i] - l[i-1]), square(params->dt));
        CONSTRAINT(square(r[i] - r[i-1]), square(params->dt));
    }

#undef CONSTRAINT
#undef PENALTY
    y += cons + penalty;
    return y;
}

/* L-BFGS proxy function for evaluating the objective function */
static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x,
        lbfgsfloatval_t *g, int n, lbfgsfloatval_t step)
{
    int i;
    region_params_t *params = (region_params_t *)instance;

    for (i = 0; i < n; i++) {
        g[i] = diff(objective, x, params, n, i);
    }
    return objective(x, params);
}

/* L-BFGS proxy function for showing progress */
static int progress(void *instance, const lbfgsfloatval_t *x,
        const lbfgsfloatval_t *g, const lbfgsfloatval_t fx,
        const lbfgsfloatval_t xnorm, const lbfgsfloatval_t gnorm,
        const lbfgsfloatval_t step,
        int n, int k, int ls)
{
    printf("Iteration %06d (fx = %10.4f; n = %d)    \r", k, fx, n);
    fflush(stdout);
    return 0;
}

/* Try to compute a trajectory in the given region.
 *  l       Vector of length {params.N} that will receieve the left thruster
 *          values for each time step.
 *  r       Vector of length {params.N} that will receive the right thruster
 *          values for each time step.
 *  params  A structure containing the problem to solve.
 */
region_result_t compute_trajectory(double *l, double *r, region_params_t *params)
{
    lbfgsfloatval_t fx;
    lbfgs_parameter_t param;
    int i;
    int n = params->N;
    lbfgsfloatval_t *x = lbfgs_malloc(n*2);
    int ret = 0;

    if (!x) {
        printf("ERROR: Failed to allocate a memory block for variables.\n");
        return 1;
    }

    for (i = 0; i < n*2; i++) {
        x[i] = 0;
    }

    /* Initialize the parameters for the L-BFGS optimization. */
    lbfgs_parameter_init(&param);
    param.linesearch = LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE;
    param.past = 100;
    param.delta = 1e-4;
    param.m = 15;

    ret = lbfgs(n*2, x, &fx, evaluate, progress, params, &param);
    memcpy(l, x, sizeof(l[0])*n);
    memcpy(r, x+n, sizeof(r[0])*n);

    printf("\nL-BFGS optimization terminated with status code = %d\n", ret);
    printf("fx = %f\n", fx);
    printf("\n");

    printf("MATLAB variables:\n");
    printf("l = [ ");
    for (i = 0; i < n; i++) { printf("%f ", l[i]); }
    printf(" ];\nr = [ ");
    for (i = 0; i < n; i++) { printf("%f ", r[i]); }
    printf(" ];\n");

    printf("rx = [ ");
    for (i = 0; i < params->polycount; i++) { printf("%f ", params->poly[i].x); }
    printf(" ];\nry = [ ");
    for (i = 0; i < params->polycount; i++) { printf("%f ", params->poly[i].y); }
    printf(" ];\n");
    printf("parm = [ %f %f %f %f %f %f %f %f %f ];\n",
        params->p_0.x, params->p_0.y, params->v_0.x, params->v_0.y,
        params->d_0.x, params->d_0.y, params->omega_0, params->mass,
        params->radius);
    printf("dt = %f\n", params->dt);

    lbfgs_free(x);
    
    if (ret == 0 || ret == 1 || ret == 2) {
        return REGION_SOLVED;
    } else {
        return REGION_STUCK;
    }
}

