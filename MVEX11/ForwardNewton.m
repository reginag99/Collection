function x = ForwardNewton(alpha,time_mesh,x_initial)
%Computes the forward solution of the model problem.
    
    nodes = length(time_mesh)-1;

    MaxIter = 1000;  % maximal number of iterations in Newton method

    x = zeros(size(x_initial,1),nodes+1);

    x(:,1) = x_initial;

    dt = time_mesh(2:end)-time_mesh(1:end-1);   %Time step
    
    v = x_initial;
    for i = 1:nodes  
        tol=1;
        iter=0;
        while tol>10^(-10) && iter < MaxIter   %Newton iterations
            F= v - x(:,i) - dt(i)*Forwardfunc(v,alpha(:,i));
            J=eye(length(x_initial)) - dt(i)*ForwardfuncJacobi(v,alpha(:,i));
            dv = -J\F;
            if norm(F) <10^(-5)
%                  disp('norm(F) is very small! Newtons method stops at time step ')
%                  time_mesh(i)
                break
            end
            
            v=v+dv;              %The Newton iteration 
            iter = iter +1;
            tol = norm(dv,inf);
        end  
        if iter==MaxIter        %If the Newton meth. does not converge
            disp(['No convergence in the Newton method at time: ' num2str(time_mesh(i))])
            break
        end
        for k = 1:length(x_initial)
            if v(k) < 0
                warning('Forward solution algorithm yields invalid solution. Try increasing the number of nodes in the time partition.')
                return
            end
        end
        x(:,i+1) =  v;        
    end

end