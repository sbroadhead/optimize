param N;
param T;
param q {1..2};
param r;
param xf;

var x {j in 0..N};
var v {j in 1..N}   <= 1000, >= -1000;
var a {j in 1..N-1} <= 1000, >= -1000;
var k {1..2};

minimize trade_off: q[1]*(x[N] - xf)^2 
          + q[2]*v[N]^2
                  + r*sum{j in 1..N-1} a[j]^2 * T/N;

subject to x0: x[0] = 0;
subject to v0: v[1] = 0;

subject to v_def {j in 1..N}:   N*(x[j]-x[j-1]) = T*v[j];
subject to a_def {j in 1..N-1}: N*(v[j+1]-v[j]) = T*a[j];

subject to control {j in 1..N-1}: a[j] = k[1] + k[2]*j*T/N;
#subject to antisymmetry: k[1] = -5*k[2];

let N := 800;
let T := 10;

let q[1] := 1;
let q[2] := 1;
let r := 100;
let xf := 100;

solve;

display k;
display x[N];
display v[N];
