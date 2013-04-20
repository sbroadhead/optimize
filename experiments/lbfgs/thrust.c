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
double mu = 1e-5;

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

static double lb(double x, double y) /* log barrier */
{
    return -mu * (y <= x) ? HUGE_VAL : log(y - x);
}

static double qp(double x, double y) /* quadratic penalty */
{
    return mu * (y <= x) ? (y-x)*(y-x) : 0;
}

static double ib(double x, double y) /* inverse barrier */
{
    return mu * (y <= x) ? HUGE_VAL : 1/(y - x);
}

static double sqdist(double x, double y) { return (x-y)*(x-y); }
static double dist(double x, double y) { return fabs(x-y); }
static double sqdist2(double x1, double y1, double x2, double y2) { return sqdist(x1, x2)+sqdist(y1, y2); }
static double dist2(double x1, double y1, double x2, double y2) { return sqrt(sqdist2(x1, y1, x2, y2)); }

static double f(const double *x)
{
    const int iters = 10;
    double h = DT / (double)iters;

    double px[2], py[2], vx[2], vy[2], dx[2], dy[2], w[2];
    double ax, ay;
    double dw;
    double theta;
    double dnorm;
    double y = 0;
    double cons = 0.0;
    double pdist;
    double angle;
    int ccount = 0;
    int i, j;

    int cur = 1;
    int prev = 0;
    
    px[prev] = vals.px0; py[prev] = vals.py0;
    vx[prev] = vals.vx0; vy[prev] = vals.vy0;
    dx[prev] = vals.dx0; dy[prev] = vals.dy0;
    w[prev] = vals.w0;

    cur = 1;
    prev = 0;

#define CONSTRAINT(x, y) do{double c=ib(x,y)/(ccount+1);\
    double cc=(ccount?(cons/(1.0+1.0/ccount)):0);\
    cons=c+cc;ccount++;}while(0)

    for (i = 1; i <= N; i++) {
        for (j = 0; j < iters; j++) {
            /* angular velocity */
            dw = 2*(x[L+i-1] - x[R+i-1])/(vals.radius*vals.mass);
            w[cur] = w[prev] + dw * h;
            /* direction */
            theta = 0.5 * h * (w[cur] + w[prev]);
            dx[cur] = dx[prev] * cos(theta) - dy[prev] * sin(theta);
            dy[cur] = dx[prev] * sin(theta) + dy[prev] * cos(theta);
            dnorm = sqrt(dx[cur]*dx[cur] + dy[cur]*dy[cur]);
            dx[cur] /= dnorm;
            dy[cur] /= dnorm;

            /* acceleration*/
            ax = dx[prev] * (x[L+i-1] + x[R+i-1])/vals.mass;
            ay = dy[prev] * (x[L+i-1] + x[R+i-1])/vals.mass;
            /* velocity */
            vx[cur] = vx[prev] + ax * h;
            vy[cur] = vy[prev] + ay * h;
            /* position */
            px[cur] = px[prev] + vx[prev] * h + 0.5 * ax * h * h;
            py[cur] = py[prev] + vy[prev] * h + 0.5 * ay * h * h;
            
            /* distance from destination */
            y += h * ((px[cur] - vals.pxn) * (px[cur] - vals.pxn) + (py[cur] - vals.pyn) * (py[cur] - vals.pyn));
    
            pdist = sqdist2(px[prev], py[prev], vals.pxn, vals.pyn);
            CONSTRAINT(sqdist2(px[cur], py[cur], vals.pxn, vals.pyn), pdist + 1e-4);
            
            prev = cur;
            cur = 1 - cur;
        }
        /*CONSTRAINT(0.1, sqrt(sqdist(px[i], 15)));
        CONSTRAINT(0.1, sqrt(sqdist(py[i], 16)));*/
    }

    for (i = 1; i < N; i++) {
        CONSTRAINT(dist(x[L+i], x[L+i-1]), DT);
        CONSTRAINT(dist(x[R+i], x[R+i-1]), DT);
    }

    angle = atan2(dy[prev], dx[prev]);

    CONSTRAINT(vx[prev], 1e-4);
    CONSTRAINT(vy[prev], 1e-4);
    CONSTRAINT(angle*angle, 1e-4);
    
    CONSTRAINT(dist(x[L], vals.ls), DT);
    CONSTRAINT(dist(x[R], vals.rs), DT);

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
    /*for (i = 0; i < N; i ++){
        printf("l[%d] = %.10f; r[%d] = %.10f\n", i, x[L+i], i, x[R+i]);
    }
    printf("\n");
    printf("  xnorm = %f, gnorm = %f, step = %f\n", xnorm, gnorm, step);*/
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
    param.past = 1;
    param.delta = 1e-8;
    //param.max_linesearch = 200;
    //param.gtol = 1e-3;

    ret = lbfgs(n, x, &fx, evaluate, progress, NULL, &param);

    printf("L-BFGS optimization terminated with status code = %d\n", ret);
    printf("  fx = %f;\n", fx);
    for (i = 0; i < N; i++){
        printf("    l[%d] = %.10f; r[%d] = %.10f\n", i, x[L+i], i, x[R+i]);
    }
    printf("\n");

    printf("MATLAB vectors:\n");
    printf("l = [ ");
    for (i = 0; i < N; i++) { printf("%f ", x[L+i]); }
    printf(" ]\nr = [ ");
    for (i = 0; i < N; i++) { printf("%f ", x[R+i]); }
    printf(" ]\n");

    /* Answer according to Wolfram Alpha:
     *  1.00607 x^2 - 0.363643 x + 0.554
     */

    lbfgs_free(x);
    return 0;
}
