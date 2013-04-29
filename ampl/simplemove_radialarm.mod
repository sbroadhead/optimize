param N default 100;
param T default 10;
param g default 9.8;
param m_arm default 10;
param m_object default 1;
param m_hand default 1;
param m_fore default 5;
param d default 0.1;

param k{1..3} default 1;

param theta_0 default 0;
param l_0 default 1;
param n_0 default 1;

param theta_N default 1;
param l_N default 2;
param n_N default 1;

param hs default 1;
param hd default 1;
param rd default 1;
param dh_dr default 1;

var theta     {j in 0..N};
var thetadot  {j in 1..N}    <= 1000, >= -1000;
var thetadot2 {j in 1..N-1}  <= 1000, >= -1000;

var l         {j in 0..N};
var ldot      {j in 1..N}    <= 1000, >= -1000;
var ldot2     {j in 1..N-1}  <= 1000, >= -1000;

var n         {j in 0..N};
var ndot      {j in 1..N}    <= 1000, >= -1000;
var ndot2     {j in 1..N-1}  <= 1000, >= -1000;

var u         {i in 1..3, j in 1..N-1};

var a = sqrt(l[N]^2 + d^2 + 2*l[N]*d*cos(n[N]));
var b = d*sin(n[N])/a;

minimize J: 
    sum {j in 1..N-1} (
    k[1]*thetadot2[j]^2 + k[2]*ldot2[j]^2 + k[3]*ndot2[j]^2
    )*T/N
    ;

s.t. thetadot_def {j in 1..N}:   N*(theta[j]-theta[j-1]) = T*thetadot[j];
s.t. thetadot2_def{j in 1..N-1}: N*(thetadot[j+1]-thetadot[j])=T*thetadot2[j];

s.t. ldot_def  {j in 1..N}:   N*(l[j]-l[j-1]) = T*ldot[j];
s.t. ldot2_def {j in 1..N-1}: N*(ldot[j+1]-ldot[j]) = T*ldot2[j];

s.t. ndot_def  {j in 1..N}:   N*(n[j]-n[j-1]) = T*ndot[j];
s.t. ndot2_def {j in 1..N-1}: N*(ndot[j+1]-ndot[j]) = T*ndot2[j];

s.t. thetadot2_constr {j in 1..N-1}:
     thetadot2[j] 
     = 
     (u[1,j] + g*(m_arm*l[j]*cos(theta[j])/2
         +m_object*sqrt(l[j]^2 + d^2 +2*l[j]*d*cos(n[j]))
         *cos(theta[j] + d*sin(n[j]/sqrt(l[j]^2 + d^2 +
                           2*l[j]*d*cos(n[j])) ) )
                 )
     )
     /
     (m_arm*l[j]^2/12 + m_object*(l[j]^2 + d^2 + 2*l[j]*d*cos(n[j])));

s.t. ldot2_constr {j in 1..N-1}:
     ldot2[j] 
     =
     u[2,j]/(m_fore + m_object) + g*sin(theta[j]);

s.t. ndot2_constr {j in 1..N-1}:
     ndot2[j]
     =
     (u[3,j] + g*(m_hand+m_object)*d*cos(theta[j]+n[j]))
     /
     (m_hand*d^2/12 + m_object*d^2);

s.t. theta0: theta[0] = theta_0;
s.t. l0: l[0] = l_0;
s.t. n0: n[0] = n_0;

s.t. thetadot0: thetadot[1] = 0;
s.t. ldot0: ldot[1] = 0;
s.t. ndot0: ndot[1] = 0;

s.t. thetaN: theta[N] = theta_N;
s.t. lN: l[N] = l_N;
s.t. nN: n[N] = n_N;

s.t. thetadotN: thetadot[N] = 0;
s.t. ldotN: ldot[N] = 0;
s.t. ndotN: ndot[N] = 0;

#s.t. wrist_height: hs - l[N]*sin(theta[N]) = hd;
#s.t. wrist_range :      l[N]*cos(theta[N]) = rd;
#
#s.t. hand_height: hs - a*sin(theta[N] + b) = hd;
#s.t. hand_range :      a*cos(theta[N] + b) = rd;
#s.t. hand_perp  : theta[N] + n[N] = atan(dh_dr);

solve;
