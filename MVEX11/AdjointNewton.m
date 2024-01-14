%Computes the adjoint solution of the model problem.
function [lambda] = AdjointNewton(alpha, x, g, time_mesh, obs_start, obs_end)

    final_time = time_mesh(end); % here we choose the final time
    nodes = length(time_mesh);

    t=final_time;  % final  time in adjoint solver, the same final time is in the forward problem
    MaxIter = 1000;  % maximal number of iterations in Newton's method
    
    % values for lambda(T)= 0 at the final time
    lambda = zeros(length(x(:,1)),nodes);
    w = lambda(:,end);

    dt = time_mesh(2:end)-time_mesh(1:end-1);   %Time step

    %i = nodes + 1;
    for i = nodes:-1:2   
        adjtol=1;adjiter=0;
        while adjtol>10^(-5) && adjiter < MaxIter   %Newton iterations
           if t >= obs_start & t <= obs_end % z = 1
                adjF = w - lambda(:,i) + dt(i-1)*adjfunc(t,alpha(:,i),x(:,i),...
                                    lambda(:,i),g(:,i));
           else % z = 0
               adjF = w - lambda(:,i) + dt(i-1)*adj2func(t,alpha(:,i),x(:,i),...
                                    lambda(:,i));
           end
           adjJ = eye(length(lambda(:,end))) + dt(i-1)*AdjfuncJacobi(x(:,i),alpha(:,i));
           dw = -adjJ\adjF;
           w=w+dw;              %The Newton iteration 
           adjiter = adjiter +1;
           adjtol = norm(dw,inf);
        end  
        if adjiter==MaxIter        %If the Newton meth. does not converge
            disp('No convergence in the Newton method for adjoint problem')
            break
        end
        
        lambda(:,i-1) = w;        
        t=t-dt(i-1);
    end

end