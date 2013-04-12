param N > 0 := 20 integer;
param radius > 0 := 5;
param mass > 0 := 100;
param viax := 5;
param viay := 5;
param tox := 10;
param toy := 1;
param thrustchange := 2;

set ks ordered := 0..N;

var t{ks};
var dt{ks}, >= 0;
var w{ks};
var dx{ks};
var dy{ks};
var vx{ks};
var vy{ks};
var px{ks};
var py{ks};
var l{ks};
var r{ks};

minimize obj: (px[N]-tox)^2 + (py[N]-toy)^2 + 0.001 * t[N]^2; 

subject to xstart: px[0] = 0;
subject to ystart: py[0] = 0;
subject to tstart: t[0] = 0;
subject to dxstart: dx[0] = 1;
subject to dystart: dy[0] = 0;
subject to vxstart: vx[0] = 0;
subject to vystart: vy[0] = 0;
subject to wstart: w[0] = 0;
subject to rstart: r[0]^2 <= thrustchange^2;
subject to lstart: l[0]^2 <= thrustchange^2;
subject to vxend: vx[N] = 0;
subject to vyend: vy[N] = 0;
subject to dxend: dx[0] = 1;
subject to dyend: dy[0] = 0;

subject to time{k in 1..N}: t[k] = t[k-1]+dt[k-1];
subject to rotation{k in 1..N}: w[k] = dt[k-1] * 2 * (l[k-1]-r[k-1])/(mass*radius) + w[k-1]; 
subject to xdir{k in 1..N}: dx[k] = dt[k-1] * -w[k-1]*dy[k-1] + dx[k-1];
subject to ydir{k in 1..N}: dy[k] = dt[k-1] * w[k-1]*dx[k-1] + dy[k-1];
subject to xvel{k in 1..N}: vx[k] = dt[k-1] * (l[k-1]+r[k-1])*dx[k-1]/mass + vx[k-1];
subject to yvel{k in 1..N}: vy[k] = dt[k-1] * (l[k-1]+r[k-1])*dy[k-1]/mass + vy[k-1];
subject to xpos{k in 1..N}: px[k] = dt[k-1] * vx[k-1] + px[k-1];
subject to ypos{k in 1..N}: py[k] = dt[k-1] * vy[k-1] + py[k-1];

subject to ldiff{k in 1..N}: (l[k]-l[k-1])^2 <= thrustchange^2 * dt[k-1];
subject to rdiff{k in 1..N}: (r[k]-r[k-1])^2 <= thrustchange^2 * dt[k-1];
subject to dtmax{k in ks}: dt[k] <= 1;
