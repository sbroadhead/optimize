function [xbar, fbar] = bfgs(f, g, e, x0, max_iter)
    x_cur = x0;
    H = eye(rows(x0)); %% initial hessian approximation is the identity matrix
    k = 0;  %% iterate counter
    while ( norm(g(x_cur)) > e ) %% run loop as long as the size of the gradient is > epselon
        p = -H*g(x_cur);  %% search direction: Hessian * Gradient
        alpha = linesearch( f, g, x_cur, p );  %% returns step length
        x_prev = x_cur;  %% previous point = current point
        x_cur = x_prev + alpha*p;  %% next step: previous point + (step length * search direction)
        
		%% s & y will be required to calculate the new Hessian approximation
		s = x_cur - x_prev; %% difference between the points
        y = g(x_cur) - g(x_prev); %% difference between the gradients
        
		H = (eye(rows(x0)) - ((s*y')/(y'*s)))*H*(eye(rows(x0)) - ((s*y')/(y'*s))) +  ((s*s')/(y'*s)); %% new Hessian approximation
        k = k + 1; %% increment the counter
        if ( k >= max_iter ) %% exit strategy
            break;
        end
    end
    xbar = x_cur;
    fbar = f(x_cur);
endfunction