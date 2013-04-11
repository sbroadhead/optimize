function thrust()

M = 100;        % Mass in kg
R = 5;          % Radius in m

l = 5;          % Left thruster force in newtons
r = 3;          % Right thruster force in newtons

p = [0 0];      % Position
v = [0 0];      % Velocity
d = [1 0];      % Direction
omega = 0;      % Angular velocity

t = 0;          % Time
h = 0.1;        % Time step       

fig = figure( ...
    'Name', 'Thrusters', ...
    'NumberTitle', 'off');
colordef(fig, 'black');
ax = axes( ...
    'Units', 'normalized', ...
    'Drawmode', 'fast');
axis([-100 100 -100 100]);

while true
    % Make sure the direction is a unit vector
    d = d/norm(d);
    
    % Solve the ODE to get the next state
    y0 = [p(1) p(2) v(1) v(2) d(1) d(2) omega];
    result = ode45(@(t,y)(thrustODE(t,y,M,R,l,r)), [0 h], y0);
    soln = result.y(:,end);
    p = [soln(1) soln(2)];
    v = [soln(3) soln(4)];
    d = [soln(5) soln(6)];
    omega = soln(7);
    
    % Plot the graphic
    cla;
    rectangle( ...,
        'Position', [p(1)-R, p(2)-R 2*R 2*R], ...
        'Curvature', [1 1], ...
        'EdgeColor', 'white');
    line([p(1) p(1)+d(1)*R], [p(2) p(2)+d(2)*R], ...
        'Color', 'white');

    drawnow;
    
    if ~ishandle(ax)
        return
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
dy(7) = (2 * (r - l)) / (M * R);
