double obj_func(int n, const double *x, double *g) {
    double var0 = x[0];
    printf("var0 = %f\n", var0);
    double var1 = 5.0;
    printf("var1 = %f\n", var1);
    double var2 = var0-var1;
    printf("var2 = %f\n", var2);
    double var3 = log(var2);
    printf("var3 = %f\n", var3);
    double var4 = -var3;
    printf("var4 = %f\n", var4);
    double var5 = x[1];
    printf("var5 = %f\n", var5);
    double var6 = 8.0;
    printf("var6 = %f\n", var6);
    double var7 = var5-var6;
    printf("var7 = %f\n", var7);
    double var8 = log(var7);
    printf("var8 = %f\n", var8);
    double var9 = -var8;
    printf("var9 = %f\n", var9);
    double var10 = var4+var9;
    printf("var10 = %f\n", var10);
    double var11 = var0+var5;
    printf("var11 = %f\n", var11);
    double var12 = var10+var11;
    printf("var12 = %f\n", var12);
    double var13 = 1.0;
    printf("var13 = %f\n", var13);
    double var14 = var13/var2;
    printf("var14 = %f\n", var14);
    double var15 = -var14;
    printf("var15 = %f\n", var15);
    double var16 = var15+var13;
    printf("var16 = %f\n", var16);
    double var17 = var13/var7;
    printf("var17 = %f\n", var17);
    double var18 = -var17;
    printf("var18 = %f\n", var18);
    double var19 = var18+var13;
    printf("var19 = %f\n", var19);

    g[0] = var16;
    g[1] = var19;

    return var12;
}
