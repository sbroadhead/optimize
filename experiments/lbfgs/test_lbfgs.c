#include <stdio.h>
#include <lbfgs.h>
#include <math.h>

typedef struct { double x; double y; } pt;

/* Simple test problem. Compute the best least-squares quadratic polynomial to fit the following points: */
static pt points[] = {
    { 1, 1.1 }, { 2, 3.7 }, { 3, 9.04 }, { 4, 15.5 }, { 5, 22.8 }, { 6, 35.1 }
};

static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x, lbfgsfloatval_t *g,
        int n, lbfgsfloatval_t step)
{
    int i;
    double sum = 0;
    double term;
    double z;

    g[0] = 0;
    g[1] = 0;
    g[2] = 0;

    /* Objective: 
     *    minimize      sum{ (f(x_i) - y_i)^2 }
     *      where       f(x_i) = ax^2 + bx + c
     *
     * Gradient:
     *    grad((f(x_i) - y_i)^2) = 2*(f(x_i) - y_i)*grad(f(x_i))
     *    df/da = x^2
     *    df/db = x
     *    df/dc = 1
     */

    for (i = 0; i < sizeof(points)/sizeof(points[0]); i++) {
        z = points[i].x;
        term = (x[0] * z * z + x[1] * z + x[2]) - points[i].y;
        sum += term * term;

        /* Gradient */
        g[0] += 2 * term * z * z;
        g[1] += 2 * term * z;
        g[2] += 2 * term;
    }

    return sum;
}

static int progress(void *instance, const lbfgsfloatval_t *x, const lbfgsfloatval_t *g, const lbfgsfloatval_t fx,
        const lbfgsfloatval_t xnorm, const lbfgsfloatval_t gnorm, const lbfgsfloatval_t step,
        int n, int k, int ls)
{
    printf("Iteration %d:\n", k);
    printf("  fx = %f, a = %f, b = %f, c = %f\n", fx, x[0], x[1], x[2]);    
    printf("  xnorm = %f, gnorm = %f, step = %f\n", xnorm, gnorm, step);
    printf("\n");
    return 0;
}

int main(int argc, char *argv[])
{
    int i, ret = 0;
    lbfgsfloatval_t fx;
    lbfgsfloatval_t *x = lbfgs_malloc(3);
    lbfgs_parameter_t param;

    if (!x) {
        printf("ERROR: Failed to allocate a memory block for variables.\n");
        return 1;
    }

    for (i = 0; i < 3; i++) {
        x[i] = 0;
    }

    /* Initialize the parameters for the L-BFGS optimization. */
    lbfgs_parameter_init(&param);
    param.linesearch = LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE;

    ret = lbfgs(3, x, &fx, evaluate, progress, NULL, &param);

    printf("L-BFGS optimization terminated with status code = %d\n", ret);
    printf("  fx = %f, a = %f, b = %f, c = %f\n", fx, x[0], x[1], x[2]);    

    /* Answer according to Wolfram Alpha:
     *  1.00607 x^2 - 0.363643 x + 0.554
     */

    lbfgs_free(x);
    return 0;
}
