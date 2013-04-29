# Objective: linear
# Constraints: nonconvex quadratic equality
# Feasible set: nonconvex

param n;        # number of intervals in which time is discretized into

param max_speed;    
param max_force;
param x0;       # initial position
param xn;       # final   position
param v0;       # initial velocity
param vn;       # final   velocity

set Nx := {0..n};       # discrete times for position
set Nv := {0.5..n-0.5 by 1};    # discrete times for velocity
set Na := {1..n-1};     # discrete times for acceleration

var x {Nx} ;                # position
var v {Nv} <= max_speed, >= -max_speed; # velocity
var a {Na} <= max_force, >= -max_force; # acceleration
var T >=0;              # total time

minimize time: T;

subject to vel_def {i in Nv}: n*(x[i+0.5] - x[i-0.5]) = T*v[i];
subject to acc_def {i in Na}: n*(v[i+0.5] - v[i-0.5]) = T*a[i];

subject to init_pos: x[0] = x0;
subject to finl_pos: x[n] = xn;

subject to init_vel: v[0.5]   = v0;
subject to finl_vel: v[n-0.5] = vn;

data;

param n := 100;
param max_speed := 5;
param max_force := 1;
param x0 := 0;
param xn := 100;
param v0 := 0;
param vn := 0;

#let T := 100;

solve;

option display_eps 0.0001;

display x;
display v;
display a;

