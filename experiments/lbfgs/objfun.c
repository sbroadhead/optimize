double obj_func(const double *x, double *g, double mu) {
    /* VARIABLES */
    /* x[0] : x */
    /* x[1] : y */

    double var0 = mu;
    double var1 = x[0];
    double var2 = 5.0;
    double var3 = var1-var2;
    double var4 = log(var3);
    double var5 = -var4;
    double var6 = var0*var5;
    double var7 = x[1];
    double var8 = 8.0;
    double var9 = var7-var8;
    double var10 = log(var9);
    double var11 = -var10;
    double var12 = var0*var11;
    double var13 = var6+var12;
    double var14 = var1+var7;
    double var15 = var13+var14;
    double var16 = 1.0;
    double var17 = var16/var3;
    double var18 = -var17;
    double var19 = var0*var18;
    double var20 = var19+var16;
    double var21 = var16/var9;
    double var22 = -var21;
    double var23 = var0*var22;
    double var24 = var23+var16;

    g[0] = var20;
    g[1] = var24;

    return var15;
}
