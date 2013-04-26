/********************************************************************
 *
 * main.c
 *
 * The main interface to the trajectory solver.
 *
 ********************************************************************/

#include "region.h"
#include "util.h"

#define NUM_STEPS 20

int main(int argc, char *argv[])
{
    region_params_t p;
    double l[NUM_STEPS], r[NUM_STEPS];

    p.mass = 20;
    p.radius = 1;
    p.p_0 = vec_make(0, 0);
    p.v_0 = vec_make(0, 0);
    p.d_0 = vec_make(1, 0);
    p.omega_0 = 0;
    p.l_0 = 0;
    p.r_0 = 0;
    p.p_n = vec_make(6, 6);
    p.N = NUM_STEPS;
    p.constraint_mu = 0.6;
    p.penalty_mu = 10e3;
    p.dt = 1.0;

    p.polycount = 4;
    p.poly[0] = vec_make(-1.2, -14.2);
    p.poly[1] = vec_make(15.2, -1.2);
    p.poly[2] = vec_make(18.2, 8.2);
    p.poly[3] = vec_make(-1.2, 7.2);

    compute_trajectory(l, r, &p);
    return 0;
}
