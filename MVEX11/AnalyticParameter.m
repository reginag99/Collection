function [dm1_guess,g_obs] = AnalyticParameter(alpha, time_mesh,obs_start,obs_end,noise_flag,noise_level,x_initial)
                    
    %final_time = time_mesh(end);
    nodes = length(time_mesh);
    time_step = time_mesh(2:end) - time_mesh(1:end-1);              %Time step for discrete derivatives.
    g_prim = zeros(3,nodes);
    
    %Simulate the true values of the model problem, with and without noise.
    [g,g_brus,g_add] =  ExactNewton(alpha,time_mesh,noise_level,x_initial);
    if noise_flag == 1
        g_obs = g_brus;
    elseif noise_flag == 2
        g_obs = g_add;
    else
        g_obs = g;
    end

    %Compute derivatives of g.
    
    g_prim(:,2:end) = (g_obs(:,2:end)-g_obs(:,1:end-1))./time_step;

    %Compute analytic eta inside observation interval.
    r = 0.93;
    beta_T = 3*10^9;
    
    d_m2 = alpha(2,:);
    x_T = g_obs(1,:); x_M1 = g_obs(2,:); x_M2 = g_obs(3,:);
    x_T_prim = g_prim(1,:);
    
    dm1 = (r*x_T.*(1 - x_T/beta_T) + d_m2.*x_M2.*x_T - x_T_prim)./(x_M1.*x_T);
    
    dm1_guess = dm1;

    % plot(time_mesh,dm1_guess,'o')
end