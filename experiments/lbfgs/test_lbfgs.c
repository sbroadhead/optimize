#include <stdio.h>
#include <lbfgs.h>
#include <math.h>

#include "objfun.c"

static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x, lbfgsfloatval_t *g,
        int n, lbfgsfloatval_t step)
{
    return obj_func(n, x, g);
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
        x[i] = 1;
    }

    /* Initialize the parameters for the L-BFGS optimization. */
    lbfgs_parameter_init(&param);
    //param.linesearch = LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE;

    ret = lbfgs(3, x, &fx, evaluate, progress, NULL, &param);

    printf("L-BFGS optimization terminated with status code = %d\n", ret);
    printf("  fx = %f, a = %f, b = %f, c = %f\n", fx, x[0], x[1], x[2]);    

    /* Answer according to Wolfram Alpha:
     *  1.00607 x^2 - 0.363643 x + 0.554
     */

    lbfgs_free(x);
    return 0;
}
