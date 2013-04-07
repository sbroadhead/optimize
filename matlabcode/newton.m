function [xbar, fbar] = newton( f, g, h, e, x0, max_iter )
    
count = 0;

    x_cur = x0;

    x_pre = x0 .+ 2*e;

    while ( (norm(x_cur - x_pre) > e) && (count <= max_iter) )

        x_pre = x_cur;

        x_cur = x_cur - inv(h(x_cur))*g(x_cur);

        count = count + 1;

        fprintf('\t%d : f = %5.7e, x = [', count, f(x_cur) )

        for i = 1:rows(x_cur)

            fprintf('%5.7e ', x_cur(i) )

        endfor

        fprintf("]\n");

    endwhile;

    xbar = x_cur;

    fbar = f( x_cur );

endfunction;