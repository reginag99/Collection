%Computes the exact solution of the model problem.
function [g,g_brus,g_add] = ExactNewton(alpha, time_mesh,noise_level,x_initial)%(alpha, time_mesh,obs_start,obs_end,noise_level,x_initial)

    
    nodes = length(time_mesh); % Number of nodes in the time partition

    g = ForwardNewton(alpha,time_mesh,x_initial);
%     lb = find(obs_start < time_mesh,1)-1;
%     ub = nodes - find(obs_end > flip(time_mesh),1) + 2;
%     g(:,1:lb) = 0; g(:,ub:end) = 0;
    
    brus = (rand(3,nodes)*2 - 1)*noise_level;
    g_brus = g + g.*brus;

    g_add = g + g*noise_level;
end

