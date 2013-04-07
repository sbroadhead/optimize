function alpha = linesearch( f, g, x, dir )
    
    i = 0;
    mu1 = 0.2;
    mu2 = 0.8;
    rho1 = 0.5;
    rho2 = 1.5;
    alpha = 1;
    
    while ( true )
        if ( (-alpha * mu1 * g(x)'*dir) > f(x) - f(x + alpha*dir) )
            alpha = rho1*alpha;
            i = i + 1;
            continue
        end
        if ( (f(x) - f(x + alpha*dir)) > (-alpha*mu2*g(x)'*dir) )
            alpha = rho2*alpha;
            i = i + 1;
            continue
        end
        break;
    end
end
