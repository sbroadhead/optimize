#include <stdio.h>
#include <lbfgs.h>
#include <math.h>

#define N 30
#define DT 1

#define L 0
#define R (N)

typedef struct {
    double px0, py0;
    double vx0, vy0;
    double dx0, dy0;
    double ls, rs;
    double pxn, pyn;
    double w0;
    double radius;
    double mass;
} vals_t;

vals_t vals;
double mu = 1e-3;

static double grad(double (*func)(const double *), const double *x, int var)
{
    const double h = 1e-6;
    const double hinv = 1.0/h;
    double *xx = (double *)x;
    double f1, f2;
    double old = xx[var];

    xx[var] = old+h;
    f1 = func(xx);
    xx[var] = old-h;
    f2 = func(xx);
    xx[var] = old;

    return 0.5 * hinv * (f1 - f2);
}

static double lb(double x, double y)
{
    return -mu * (y <= x) ? HUGE_VAL : log(y - x);
}

static double qp(double x, double y)
{
    return mu * (y <= x) ? (y-x)*(y-x) : 0;
}

static double ib(double x, double y)
{
    return mu * (y <= x) ? HUGE_VAL : 1/(y - x);
}

static double f(const double *x)
{
    double px[N], py[N], vx[N], vy[N], dx[N], dy[N], w[N];
    double ax, ay;
    double dw;
    double theta;
    double dist;
    double dnorm;
    double y = 0;
    double cons = 0.0;
    int ccount = 0;
    int i;
    
    px[0] = vals.px0; py[0] = vals.py0;
    vx[0] = vals.vx0; vy[0] = vals.vy0;
    dx[0] = vals.dx0; dy[0] = vals.dy0;
    w[0] = vals.w0;

#define CONSTRAINT(x, y) do{double c=ib(x,y)/(ccount+1);\
    double cc=(ccount?(cons/(1.0+1.0/ccount)):0);\
    cons=c+cc;ccount++;}while(0)

    for (i = 1; i < N; i++) {
        /* angular velocity */
        dw = 2*(x[L+i-1] - x[R+i-1])/(vals.radius*vals.mass);
        w[i] = w[i-1] + dw * DT;
        /* direction */
        theta = 0.5 * DT * (w[i] + w[i-1]);
        dx[i] = dx[i-1] * cos(theta) - dy[i-1] * sin(theta);
        dy[i] = dx[i-1] * sin(theta) + dy[i-1] * cos(theta);
        dnorm = sqrt(dx[i]*dx[i] + dy[i]*dy[i]);
        dx[i] /= dnorm;
        dy[i] /= dnorm;

        /* acceleration*/
        ax = dx[i-1] * (x[L+i-1] + x[R+i-1])/vals.mass;
        ay = dy[i-1] * (x[L+i-1] + x[R+i-1])/vals.mass;
        /* velocity */
        vx[i] = vx[i-1] + ax * DT;
        vy[i] = vy[i-1] + ay * DT;
        /* position */
        px[i] = px[i-1] + vx[i-1] * DT + 0.5 * ax * DT * DT;
        py[i] = py[i-1] + vy[i-1] * DT + 0.5 * ay * DT * DT;
        
        /* distance from destination */
        dist = ((px[i] - vals.pxn) * (px[i] - vals.pxn) + (py[i] - vals.pyn) * (py[i] - vals.pyn))/N;
        y += dist;

        dist = x[L+i] - x[L+i-1];
        CONSTRAINT(dist*dist, DT*DT);
        dist = x[R+i] - x[R+i-1];
        CONSTRAINT(dist*dist, DT*DT);
    }

    CONSTRAINT(vx[N-1], 1e-4);
    CONSTRAINT(vy[N-1], 1e-4);
    dist = x[L] - vals.ls;
    CONSTRAINT(dist*dist, DT*DT);
    dist = x[R] - vals.rs;
    CONSTRAINT(dist*dist, DT*DT);

#undef CONSTRAINT
    y += cons;

    return y;
}

static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x, lbfgsfloatval_t *g,
        int n, lbfgsfloatval_t step)
{
    int i;
    for (i = 0; i < n; i++) {
        g[i] = grad(f, x, i);
    }
    return f(x);
}

static int progress(void *instance, const lbfgsfloatval_t *x, const lbfgsfloatval_t *g, const lbfgsfloatval_t fx,
        const lbfgsfloatval_t xnorm, const lbfgsfloatval_t gnorm, const lbfgsfloatval_t step,
        int n, int k, int ls)
{
    int i;

    printf("Iteration %d:\n", k);
    printf("  fx = %f;\n ", fx);
    for (i = 0; i < N; i ++){
        printf("l[%d] = %.10f; r[%d] = %.10f\n", i, x[L+i], i, x[R+i]);
    }
    printf("\n");
    printf("  xnorm = %f, gnorm = %f, step = %f\n", xnorm, gnorm, step);
    printf("\n");
    return 0;
}

int main(int argc, char *argv[])
{
    int i, ret = 0;
    int n = 2*N;
    lbfgsfloatval_t fx;
    lbfgsfloatval_t *x = lbfgs_malloc(n);
    lbfgs_parameter_t param;

    vals.mass = 100;
    vals.radius = 4;
    vals.pxn = 15;
    vals.pyn = 0;
    vals.px0 = 0;
    vals.py0 = 0;
    vals.vx0 = 0;
    vals.vy0 = 0;
    vals.dx0 = 1;
    vals.dy0 = 0;
    vals.w0 = 0;
    vals.ls = 0;
    vals.rs = 0;

    if (!x) {
        printf("ERROR: Failed to allocate a memory block for variables.\n");
        return 1;
    }

    for (i = 0; i < n; i++) {
        x[i] = 0;
    }

    /* Initialize the parameters for the L-BFGS optimization. */
    lbfgs_parameter_init(&param);
    param.linesearch = LBFGS_LINESEARCH_BACKTRACKING_WOLFE;
    //param.max_linesearch = 200;
    //param.gtol = 1e-3;

    ret = lbfgs(n, x, &fx, evaluate, progress, NULL, &param);

    printf("L-BFGS optimization terminated with status code = %d\n", ret);
    printf("  fx = %f;\n", fx);
    for (i = 0; i < N; i ++){
        printf("    l[%d] = %.10f; r[%d] = %.10f\n", i, x[L+i], i, x[R+i]);
    }
    printf("\n");

    /* Answer according to Wolfram Alpha:
     *  1.00607 x^2 - 0.363643 x + 0.554
     */

    lbfgs_free(x);
    return 0;
}
