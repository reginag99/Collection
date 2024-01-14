%% Specify fixed parameter values.
clear all
close all
clc
%Time parameters ----------------------------------------------------------
final_time = 20;
%obs_start = 100;            %Starting time for observations of true solution.
obs_start = 0;   
obs_end = 20;            %End time for observations of true solution.
%Parameters from the model problem ----------------------------------------


dm1=10^-11;
dm2 =10^-12;
at1 =10^-10;
at2 =10^-12;
k12=10^-12;
 
%Optimization parameters --------------------------------------------------
gamma_start =10^(-11);         %Initial value of regularization parameter.
opt_step_length = 0.001;  %Step length in optimization algorithm.
optim_maxiter = 500;      %Maximum iterations of optimization alg.
%Noise --------------------------------------------------------------------
noise_level = 0.01;   %Noise level of observations.
%noise_level = 0.1;   %Noise level of observations.
noise_flag = 1;       % 0 = no noise, 1 = random noise, 2 = additive noise.

% Create initial time mesh, observations and starting guess for parameter dm1.
%time_mesh = 0:final_time; % will be adaptively updated.
m=15;  %number of observations
time_mesh =linspace(0,final_time ,m);

refined = time_mesh;


%Values for   scaling factors for parameters -------------------------------------------------

scaling_factor_dm1 = 10^-11;
scaling_factor_dm2 = 10^-12;
scaling_factor_at1 = 10^-10;
scaling_factor_at2 = 10^-12;
scaling_factor_k12 = 10^-12;


function_flag = 0;       %dm1(t) = scaling_factor*function
%function flag can be between 1 and 4, determines model function which should be reconstructed
%Function flag: 0. const. 1. +linear 2. -linear 3. sin 4. exp(-x)
%NB! For now, we will specify these values inside the time-adaptive loop

%instad.
%They will be moved back here at a later stage.
%[dm1_guess,g] = AnalyticDm1(exact_dm1,time_mesh,obs_start,obs_end,noise_flag,noise_level);
%Input: 1. exact dm1 2. time mesh 3. observation start 4. observation end. 5. noise_flag. 6. noise level

% initials for forward problem
x_initial = [5*10^6; 10^3; 10^3]; 

%===============  values of exact constant parameters
   
eps = 10^(-7);
% Run algorithm
    figure
for meshref = 1:10  %Outer loop is for time adaptivity, will be replaced with while loop later.
			  
	time_mesh = refined;                                        %Our new time mesh (if mesh refinement has been made.
											    
	nodes = length(time_mesh);
	%res_time= 0.1*ones(1,nodes);										    
    

    exact_dm1 = ExactParameter(scaling_factor_dm1,function_flag,time_mesh); %Exact profile for dm1 to produce data.
    exact_dm2 = ExactParameter(scaling_factor_dm2,function_flag,time_mesh); %Exact profile for dm2 to produce data.
    exact_at1 = ExactParameter(scaling_factor_at1,function_flag,time_mesh); %Exact profile for at1 to produce data.
	exact_at2 = ExactParameter(scaling_factor_at2,function_flag,time_mesh); %Exact profile for at2 to produce data.
    exact_k12 = ExactParameter(scaling_factor_k12,function_flag,time_mesh); %Exact profile for k12 to produce data.
	    
    alpha = [exact_dm1; exact_dm2; exact_at1; exact_at2; exact_k12];
											    
    %Observations and first guess for dm1.											    
	[dm1_guess,g] = AnalyticParameter(alpha, time_mesh,obs_start,obs_end,noise_flag,noise_level,x_initial);

    %plot(time_mesh,dm1_guess,'o')
    %obtain initial guess for gradient method using method of normal equations

     %use polynomial of degree 5 to recover noisy data via method of normal equations and obtain initial guess
     %dm1_guess=LinearClassAdapt(dm1_guess, time_mesh,m);
    dm1_guess = LinearClassNormalEqExample2(dm1_guess,time_mesh,m);   
    dm1 = zeros(optim_maxiter,nodes);                           %Preallocation of dm1 (for use in for-loop).
										    
										    
    dm1(1,:) = dm1_guess;                                       %First guess for dm1 in first row.
    beta = opt_step_length*ones(1,nodes);                       %
    gamma = gamma_start;                                        %Restore beta and gamma.
    big_grad = zeros(1,nodes);                                  %Preallocation of vector for adaptivity.
    u1 = ForwardODE45(alpha,time_mesh,x_initial);                     %Compute initial forward sol.
    lambda1 = AdjointODE45(alpha,u1,g,time_mesh,obs_start,obs_end);   %Compute adjoint sol.

    % u1(1,:) = X_T; u1(2,:) = X_M1; u1(3,:) = X_M2;
											    
    g0 = (lambda1(1,:).*u1(1,:).*u1(2,:));            %Compute grad with respect to dm1  without reg.term

											    
    grad_hist = zeros(optim_maxiter,nodes);
    grad_hist(1,:) = g0;                                        %Save gradient for later plot.
    if norm(g0) < 1000
        dm1(1:end,:) = ones(length(1:optim_maxiter),1)*dm1(1,:);    %Check if gradient is already small
        disp('The algorithm has converged at first step!')
        break
    end
    bm = 1/gamma;                                               %For Conjugate Gradient algorithm.
    gn = -g0;
    dn = gn;
    g0 = sign(g0);                                                  %Normalize gradient.
    dm1(2,:) = dm1(1,:) + beta.*g0;                                 %Compute new dm1.
    iter = 0;
    for i = 2:optim_maxiter-1                                       %Here we start calculating dm1.
        gamma = gamma/i^0.5;                                        %Update gamma.
        u = ForwardODE45(alpha,time_mesh,x_initial);                      %Update forward and adjoint sol.
        lambda = AdjointODE45(alpha,u,g,time_mesh,obs_start,obs_end);

	  % update gradient for dm1  with reg.term: gamma is reg.parameter	  
        gm = gamma*(dm1(i,:)-dm1(1,:)) + lambda1(1,:).*u1(1,:).*u1(2,:);       
	
        bs = (norm(gm)/norm(gn))^2;                                 %For CG.
        dm = -gm + bs*dn;
        bm = -(gm*dn')/(gamma*(dn*dn'));                            %beta = -<g,d>/gamma<d,d>
        grad_hist(i,:) = gm;                                        %Save gradient for later plot.
        if norm(gm) < 0.001                                         %Check if gradient is small enough.
            dm1(i+1:end,:) = ones(length(i+1:optim_maxiter),1)*dm1(i,:);
            grad_hist(i+1:end,:) = ones(length(i+1:optim_maxiter),1)*grad_hist(i,:);
            disp('The algorithm has converged!')
            break
        elseif 0.999999*norm(dm1(i-1,:)) < norm(dm1(i,:)) && norm(dm1(i,:)) < 1.000001*norm(dm1(i-1,:))
            dm1(i+1:end,:) = ones(length(i+1:optim_maxiter),1)*dm1(i,:);
            grad_hist(i+1:end,:) = ones(length(i+1:optim_maxiter),1)*grad_hist(i,:);
            disp('Dm1 does not change!')                            %Also terminate if dm1 stabilizes.
            break
        %elseif norm(grad_hist(i-1,:)) < norm(grad_hist(i,:))
        %    dm1(i+1,:) = dm1(i,:) + 0.5*bdm1.*sign(grad_hist(i,:));
        %    dm1(i+2:end,:) = ones(length(i+2:optim_maxiter),1)*dm1(i+1,:);
        %    disp('Gradient grows.')
        %    break
        %    %Termination if the gradient increases turned out to give
        %    worse results, and is therefore disabled.
        else
            gm = sign(gm);                                      %Otherwise, continue with line search.
            for j = 1:nodes
                if dm1(i,j) < 10^-9                                 %Force dm1=0 if computed dm1 is negative.
                    dm1(i+1,j) = 10^-9;
                elseif dm1(i,j) > 10^-3
                    dm1(i+1,j) = 10^-3;
                elseif bm < 0.01
                    dm1(i+1,j) = dm1(i,j) + bm*gm(j);           %Use conjugate gradient if appropriate.
                else
                    if gm(j) == -gn(j)                          %Otherwise use fixed step length.
                        %bdm1(j) = beta(j)/2;                    %If gradient change sign, halve step-length.
                        beta(j) = beta(j)/2;
                    end
                    dm1(i+1,j) = dm1(i,j) - beta(j)*gm(j);      %Compute new dm1 closer to the actual solution. 
                end
            end
        end
        gn = gm;                %Save gradient for use in the CG algorithm.
        dn = dm;
        iter = iter + 1;
    end


    iter;
    max_grad = max(abs(grad_hist(end,:)));
    big_grad = abs(grad_hist(end,:)) > 0.2*max_grad;

    refined = [];               %Start refinement of time mesh ...
    refining = [];
    k = 1;
    r = 1;
    while r < length(big_grad)
        if big_grad(r+1) == 1 && big_grad(r) == 0
            refined = [refined,time_mesh(k:r)];
        end
        if big_grad(r) == 0
            r = r + 1;
            if r == length(big_grad)
                refined = [refined,time_mesh(k:r)];
            end
            continue
        else
            %j = i;
            while big_grad(r) == 1 && r < length(time_mesh)
                refining = [refining,time_mesh(r),(time_mesh(r)+time_mesh(r+1))/2];
                r = r + 1;
                if r == length(time_mesh)
                    refining = [refining,time_mesh(r)];
                end
            end
            refined = [refined, refining];
            k = r;
        end
        refining = [];
    end
    
    dm1_final = dm1(end,:);


    %relativeerror = norm((dm1(end,:)-exact_dm1)/exact_dm1)/m;
    relativeerror = norm((dm1(end,:)-exact_dm1)/exact_dm1);
    reler(meshref) = relativeerror;

             

    %if (meshref > 1 & relativeerror < reler(meshref-1) )
    figure

    plot(time_mesh,exact_dm1,'b','LineWidth',3)
    hold on
    plot(time_mesh,dm1_final,'-*','LineWidth',1)

    title(['Calculated dm1, number of refinements: ',num2str(meshref),'number of points',num2str(m)])		   
    legend('exact dm1', 'computed dm1 (CGM)')
    
    hold off



    %figure(2)

    %plot(time_mesh,exact_dm1,'b','LineWidth',3)

    %hold on


    % plot(time_mesh,dm1_final,'-*','LineWidth',1)

    %    title(['Calculated \dm1, number of refinements: ',num2str(meshref),'number of  points',num2str(m)])		   
    %      legend('exact \dm1','computed \dm1') 


    %    hold off


    meshref
      %  plot(time_mesh, dm1(end,:)) %Plot calculated dm1 for each iteration in time adaptive algorithm.

    %end
    reler
        %include time partitioning
    %% Visualize   computed dm1 
    %figure
    %surf(dm1)
    %xlabel('time partition');
    %ylabel('iteration');
    %zlabel('dm1');
    %title('Computed drug efficiency, dm1');


    % presentation of the computed gradient
    %figure
    %surf(grad_hist)
    %xlabel('time partition');
    %ylabel('iteration');
    %zlabel('gradient');
    %title('Gradient of the functional');




    if ( relativeerror < eps)
                  break
    end


      if (meshref > 1 & relativeerror >  reler(meshref-1) )
                      break
                  end

    %% Apply least-squares smoothing on the solution

    %dm1_final = dm1(end,:);
    N = length(dm1_final);
    e = ones(N,1);
    D = spdiags([e -2*e e], 0:2, N-2, N);
    F = speye(N) + 50*(D'*D);
    dm1_smooth = F\dm1_final';

    figure
    plot(time_mesh,exact_dm1,'LineWidth',2)

               hold on
    plot(time_mesh,dm1_guess,'LineWidth',2)

    plot(time_mesh,dm1_smooth,'LineWidth',2)
               legend('exact dm1','guess for dm1 (LS)','computed dm1 (smoothed CGM)')



    %	plot(time_mesh,dm1_final,'LineWidth',2)
    %		   legend('exact \dm1','guess for \dm1','computed \dm1')


               xlabel('Time')


    title(['Calculated dm1, number of refinements: ',num2str(meshref),'number of  points',num2str(m)])	

    hold off
end
