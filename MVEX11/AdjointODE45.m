function lambda = AdjointODE45(alpha, x, g, time_mesh,obs_start,obs_end)
    [time,lambda] = ode45(@(t,lambda) ...
                Adjfunc(t,lambda,x,g,alpha,time_mesh,obs_start,obs_end),...
                flip(time_mesh),[0;0;0]);
    lambda = flip(lambda)';
    
    function func = Adjfunc(t,lambda,x,g,alpha,time_mesh,obs_start,obs_end)
        alphaPol =@(t) [];
        for i = 1:size(alpha,1)
           Pol_i =@(t) interp1(time_mesh,alpha(i,:),t);
           alphaPol =@(t) [alphaPol(t); Pol_i(t)]; 
        end
        alphat = alphaPol(t);

        xPol =@(t) [];
        for i = 1:size(x,1)
           Pol_i =@(t) interp1(time_mesh,x(i,:),t);
           xPol =@(t) [xPol(t); Pol_i(t)]; 
        end
        xt = xPol(t);
        
        gPol =@(t) [];
        for j = 1:size(g,1)
           Pol_j =@(t) interp1(time_mesh,g(j,:),t);
           gPol =@(t) [gPol(t); Pol_j(t)]; 
        end
        
        if t >= obs_start & t <= obs_end % z = 1
               gt = gPol(t);
               func = adjfunc(t,alphat,xt,...
                                    lambda,gt);
           else % z = 0
               func = adj2func(t,alphat,xt,...
                                    lambda);
           end
    end
    
    
end


