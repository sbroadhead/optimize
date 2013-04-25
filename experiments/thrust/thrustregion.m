% thrustregion
%   l      Left thrust force at each time step
%   r      Right thrust force at each time step
%   rx     Feasible region polygon x coordinates
%   ry     Feasible region polygon y coordinates
%   dt     Length of the time step
%   parm   Initial parameters
%    1 2       Initial position
%    3 4       Initial velocity
%    5 6       Initial direction
%      7       Initial angular velocity
%      8       Mass
%      9       Radius
function thrustregion(l, r, rx, ry, dt, parm)

M = parm(8);         % Mass in kg
R = parm(9);          % Radius in m

p = [parm(1) parm(2)];      % Position
v = [parm(3) parm(4)];      % Velocity
d = [parm(5) parm(6)];      % Direction
omega = parm(7);      % Angular velocity

iter = 10;      % ODE solver iterations
h = dt/iter;    % Iteration step size

% Inputs:
%   T : time steps
%   p_dest : destination
%   p_via : point to go near
% Independent Variables:
%   l_k, r_k, dt_k                                    for k = 1 to T
% Dependent Variables:
%   px_k, py_k, vx_k, vy_k dx_k, dy_k, w_k, h_k, t_k  for k = 1 to T
% Constraints:
%   t_0   = 0
%   t_k   = t_{k-1} + dt_{k-1}
%   w_k   = w(t_k)
%   h_k   = h(t_k)
%   dx_k  = dx(t_k)
%   dy_k  = dy(t_k)
%   vx_k  = vx(t_k)
%   vy_k  = vy(t_k)
%   px_k  = px(t_k)
%   py_k  = py(t_k)
% Objective:
%   minimize ||p_k - p_dest|| + sum{ dt_i + ||p_i-p_via|| }

fig = figure( ...
    'Name', 'Thrusters', ...
    'NumberTitle', 'off');
colordef(fig, 'black');
ax = axes( ...
    'Units', 'normalized', ...
    'Drawmode', 'fast');
axis([-20 20 -20 20]);
axis square

xs = [];
ys = [];

count = length(l);
for i=2:count+1
    for j=1:iter
        % Solve the ODE to get the next state
        y = [p(1) p(2) v(1) v(2) d(1) d(2) omega];
        
        %result = ode45(@(t,y)(thrustODE(t,y,M,R,l(i),r(i))), [0 h], y0);
        soln = step(h,y,M,R,l(i-1),r(i-1));
        %soln = result.y(:,end);
        p = [soln(1) soln(2)];
        v = [soln(3) soln(4)];
        d = [soln(5) soln(6)];
        omega = soln(7);
        
        xs = [xs p(1)];
        ys = [ys p(2)];

        hold on
        
        % Plot the graphic
        cla;
        fill(rx, ry, [0.3 0.3 0.3]);
        rectangle( ...,
            'Position', [p(1)-R, p(2)-R 2*R 2*R], ...
            'Curvature', [1 1], ...
            'EdgeColor', 'white');
        line([p(1) p(1)+d(1)*R], [p(2) p(2)+d(2)*R], ...
            'Color', 'white');
        status = [sprintf('p:(%f %f)\n', p(1), p(2)) sprintf('v:(%f %f)\n', v(1), v(2)) sprintf('lr:(%f %f)\n', l(i-1), r(i-1))];
        text(-15, 5, status);
     
        plot(xs, ys);
       
        drawnow;
    end
end

% System of first order ODEs to simulate the thrust
function dy = thrustODE(t,y,M,R,l,r)
dy = zeros(5, 1);
dy(1) = y(3);           % p_x' = v_x
dy(2) = y(4);           % p_y' = v_y
dy(3) = (l+r)*y(5)/M;   % v_x' = a_x = (l+r)d_x/M;
dy(4) = (l+r)*y(6)/M;   % v_x' = a_y = (l+r)d_y/M;
dy(5) = -y(7) * y(6);   % d_x' = -omega * d_y
dy(6) = y(7) * y(5);    % d_y' = omega * d_x
                        % omega' = 2*R*(l-r) / (M*R^2)
dy(7) = (2 * (l - r)) / (M * R);

function y = step(dt,y,M,R,l,r)
lasty = y;
dw = 2 * (l - r) / (M * R);
y(7) = lasty(7) + dw * dt;
t = 0.5 * dt * (y(7) + lasty(7));
y(5) = lasty(5) * cos(t) - lasty(6) * sin(t);
y(6) = lasty(5) * sin(t) + lasty(6) * cos(t);
dnorm = norm([y(5) y(6)]);
y(5) = y(5) / dnorm;
y(6) = y(6) / dnorm;
ax = lasty(5) * (l + r)/M;
ay = lasty(6) * (l + r)/M;
y(3) = lasty(3) + ax * dt;
y(4) = lasty(4) + ay * dt;
y(1) = lasty(1) + lasty(3) * dt + 0.5 * ax * dt * dt;
y(2) = lasty(2) + lasty(4) * dt + 0.5 * ay * dt * dt;



