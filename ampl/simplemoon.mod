# Objective: linear
# Constraints: nonconvex quadratic equality
# Feasible set: nonconvex

param pi := 4*atan(1);

param n;    # number of intervals in which time is discretized into
param T;    # total time

set D := {1..2};
set Nx := {0..n};       # discrete times for position
set Nv := {0.5..n-0.5 by 1};    # discrete times for velocity
set Na := {1..n-1};     # discrete times for acceleration

param G;
param m_earth;
param r_earth;
param x_earth{D};
param m_moon;
param r_moon;
param x_moon{D};

param x0{D};        # initial position
param xn{D};        # final   position
param v0{D};        # initial velocity
param vn{D};        # final   velocity
param alpha0;
param alphan;

var x {D, Nx};  # position
var v {d in D, i in Nv} =   n*(x[d,i+0.5] - x[d,i-0.5])/T; # velocity
var a {d in D, i in Na} =   n*(v[d,i+0.5] - v[d,i-0.5])/T; # acceleration
var theta {D, Na};  # thrust

minimize fuelcost: sum {d in D, i in Na} theta[d,i]^2;

subject to init_pos {d in D}: x[d,0] = x0[d];
subject to finl_pos {d in D}: x[d,n] = xn[d];

subject to init_vel {d in D}: v[d, 0.5]   = v0[d];
subject to finl_vel {d in D}: v[d, n-0.5] = vn[d];

subject to force_bal {d in D, i in Na}:
    a[d,i] = 
       - G*m_earth*(x[d,i]-x_earth[d])
         /(sum {dd in D} (x[dd,i]-x_earth[dd])^2)^(3/2)
           - G*m_moon*(x[d,i]-x_moon[d])
         /(sum {dd in D} (x[dd,i]-x_moon[dd])^2)^(3/2)
       + theta[d,i];

data;

let n := 100;
let T := 1;
let G := 1;
let m_earth := 10;
let m_moon  :=  1;
let r_earth :=  2;
let r_moon  :=  1;
let x_earth[1] := 0;
let x_earth[2] := 0;
let x_moon[1]  := 20;
let x_moon[2]  := 0;
let alpha0 := 1.5*pi;
let alphan := pi/2;
let x0[1] := x_earth[1]+r_earth*cos(alpha0);
let x0[2] := x_earth[2]+r_earth*sin(alpha0);
let xn[1] := x_moon[1] +r_moon *cos(alphan);
let xn[2] := x_moon[2] +r_moon *sin(alphan);
let v0[1] :=  5*cos(alpha0);
let v0[2] :=  5*sin(alpha0);
let vn[1] := -5*cos(alphan);
let vn[2] := -5*sin(alphan);

let {d in D, i in Nx} x[d,i] := (1-i/n)*x0[d] + (i/n)*xn[d];

solve;

option display_eps 0.0001;

printf {i in Nx}: "%10.5f %10.5f \n", x[1,i], x[2,i] > moon.out;
printf {i in 0..100}: "%10.5f %10.5f \n", 
    x_earth[1]+r_earth*cos(2*pi*i/100),
    x_earth[2]+r_earth*sin(2*pi*i/100) > earthcircle.out;
printf {i in 0..100}: "%10.5f %10.5f \n", 
    x_moon[1]+r_moon*cos(2*pi*i/100),
    x_moon[2]+r_moon*sin(2*pi*i/100) > mooncircle.out;

printf {i in Na}: "%10.5f \n", theta[1,i]^2 + theta[2,i]^2;
