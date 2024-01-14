%%  Initials
clc; close all; clear all;
%%initial of x and time variables 
m = 15; % number of observations
obs_start = 2.1; obs_end = 7; %interval of observations
time_final = 20;

time_mesh = linspace(0,time_final,m);
% x_initial = [x_T(0); x_M1(0); x_M2(0)]
% initial values that seems to fit fig 1a 
x_initial = [5*10^6; 10^3; 10^3]; 


%% observations 
alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
noise_level = 0.1;% 10% noise

%using ode45 since newton seems to have problems with low numbers for
% this particular system of odes

[g, g_brus1, g_add] = ExactODE45(alpha,time_mesh,noise_level,x_initial);
figure

plot(time_mesh,g(1,:),'linewidth',2)
hold on
plot(time_mesh,g_brus1(1,:),'*')
  legend('x_T, ode45','observations g1')
  title(['ODE45 versus noisy data, noise , \delta= ',num2str(noise_level)]);

  
%% Test solver quality : Forward
figure
hold on
sch = 0;
for m2 = [15 20 50 100 500 1000]
    time_mesh2 = linspace(0,time_final,m2);
    alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh2);
    FN = ForwardNewton(alpha,time_mesh2,x_initial);
    F45 = ForwardODE45(alpha,time_mesh2,x_initial);
    subtitle('ODE45 versus Newton  for forward problem');
sch = sch+1;
subplot(2, 3, sch);
plot(time_mesh2,FN(1,:),'-r',time_mesh2,F45(1,:),'--b','linewidth',2);
subtitle('ODE45 versus Newton  for forward problem');
xlabel(m2)

legend('x_T Newton', 'x_T ode45')
%     text(time_mesh2(end),FN(1,end),['N', num2str(m2)])
%     text(time_mesh2(end),FN(1,end),['45', num2str(m2)])
end

grid on




%% Test solver quality : Adjoint
clc
noise_level = 0.1;
time_obs = linspace(obs_start, obs_end, 15);
alpha_obs = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_obs);
[g, g_brus, g_add] = ExactODE45(alpha_obs,time_obs,noise_level,x_initial); % get observations
%interpolate to be able to use for other time meshes
gPol =@(t) [];
for j = 1:size(g_brus,1)
   Pol_j =@(t) interp1(time_mesh,g_brus(j,:),t);
   gPol =@(t) [gPol(t); Pol_j(t)]; 
end


figure
hold on

sch = 0;

for m2 = [15 20 50 100 500 1000]
    time_mesh2 = linspace(0,time_final,m2);
    alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh2);
    g_brus = gPol(time_mesh2);
    FN = ForwardNewton(alpha,time_mesh2,x_initial);  
    F45 = ForwardODE45(alpha,time_mesh2,x_initial);  
    AN = AdjointNewton(alpha, FN, g_brus, time_mesh2, obs_start, obs_end);
    A45 = AdjointODE45(alpha, F45, g_brus, time_mesh2, obs_start, obs_end);
    sch = sch+1;
    subplot(2, 3, sch);
    plot(time_mesh2,AN(2,:),'-r',time_mesh2,A45(2,:),'--b','linewidth',2)
    %text(time_mesh2(1),A45(2,1),['', num2str(m2)])
    %text(time_mesh2(end),FN(1,end),['45', num2str(m2)])
    xlabel(m2)
    legend('x_T Newton', 'x_T ode45')
    title('ODE45 versus Newton for adjoint problem'); 
    hold on
end
   title('ODE45 versus Newton for adjoint problem'); 

grid on


    
hold off
%% Test Forward Newton vs ode45
alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN = ForwardNewton(alpha,time_mesh,x_initial);
F45 = ForwardODE45(alpha,time_mesh,x_initial);
figure
subplot(2, 3, 1);
plot(time_mesh,FN(1,:),time_mesh,F45(1,:),'--','linewidth',2)
legend('x_T Newton', 'x_T ode45')
grid on
hold on
%legend('x_T Newton', 'x_M1','x_M2','x_T ode45', 'x_M1','x_M2')

%% Newton and Raluca
%fig 1a
figure

alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN = ForwardNewton(alpha,time_mesh,x_initial);

subplot(2, 2, 1);
plot(time_mesh,FN,'--','linewidth',2)
grid on
legend('x_T', 'x_{M1}','x_{M2}')
title('(a)')
hold on

%fig 1b

alpha = alpha_vec(10^-11,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN1 = ForwardNewton(alpha,time_mesh,x_initial);
alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN2 = ForwardNewton(alpha,time_mesh,x_initial);
alpha = alpha_vec(10^-7,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN3 = ForwardNewton(alpha,time_mesh,x_initial);

subplot(2, 2, 2);
plot(time_mesh,FN1(1,:),':r',time_mesh,FN2(1,:),'-r',time_mesh,FN3(1,:),'-.r','linewidth',2)
title('(b)')
legend('d_{m1} = 10^{-11}','d_{m1} = 10^{-9}','d_{m1} = 10^{-7}')
ylim([0 4*10^9])
grid on
hold on

%fig 1c

alpha = alpha_vec(10^-9,10^-12,10^-8,10^-10,5*10^-10,time_mesh);
FN1 = ForwardNewton(alpha,time_mesh,x_initial);
alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN2 = ForwardNewton(alpha,time_mesh,x_initial);
alpha = alpha_vec(10^-9,10^-8,10^-8,10^-10,5*10^-10,time_mesh);
FN3 = ForwardNewton(alpha,time_mesh,x_initial);

subplot(2, 2, 3);
plot(time_mesh,FN1(1,:),':r',time_mesh,FN2(1,:),'-r',time_mesh,FN3(1,:),'-.r','linewidth',2)
title('(c)')
legend('d_{m1} = 10^{-11}','d_{m1} = 10^{-9}','d_{m1} = 10^{-7}')
ylim([0 4*10^9])
grid on
hold on

%fig 1d

alpha = alpha_vec(10^-9,10^-10,10^-10,10^-10,5*10^-10,time_mesh);
FN1 = ForwardNewton(alpha,time_mesh,x_initial);
alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN2 = ForwardNewton(alpha,time_mesh,x_initial);
alpha = alpha_vec(10^-9,10^-10,10^-6,10^-10,5*10^-10,time_mesh);
FN3 = ForwardNewton(alpha,time_mesh,x_initial);

subplot(2, 2, 4);
plot(time_mesh,FN1(1,:),':r',time_mesh,FN2(1,:),'-r',time_mesh,FN3(1,:),'-.r','linewidth',2)
title('(d)')
legend('d_{m1} = 10^{-11}','d_{m1} = 10^{-9}','d_{m1} = 10^{-7}')
ylim([0 4*10^9])
grid on

%% Test Adjoint newton vs ode45
alpha = alpha_vec(10^-9,10^-10,10^-8,10^-10,5*10^-10,time_mesh);
FN = ForwardNewton(alpha,time_mesh,x_initial);
F45 = ForwardODE45(alpha,time_mesh,x_initial); 
AN = AdjointNewton(alpha, FN, g_brus1, time_mesh, obs_start, obs_end);
A45 = AdjointODE45(alpha, F45, g_brus1, time_mesh, obs_start, obs_end);

figure
for i = 1:size(AN,1)
	subplot(2, 3, i);  
    plot(time_mesh,AN(i,:),'linewidth',2)
    plot(time_mesh,A45(i,:),'--','linewidth',2)
    legend(['\lambda',num2str(i),'Newton'],['\lambda',num2str(i),'ode45'])
    hold on
end


%% Inner functions

function alpha = alpha_vec(dm1,dm2,at1,at2,k12,time_mesh)
    scaling_factor_dm1 = dm1;
    scaling_factor_dm2 = dm2;
    scaling_factor_at1 = at1;
    scaling_factor_at2 = at2;
    scaling_factor_k12 = k12;

    function_flag = 0; % constant

    exact_dm1 = ExactParameter(scaling_factor_dm1,function_flag,time_mesh); %Exact profile for dm1 to produce data.
    exact_dm2 = ExactParameter(scaling_factor_dm2,function_flag,time_mesh); %Exact profile for dm2 to produce data.
    exact_at1 = ExactParameter(scaling_factor_at1,function_flag,time_mesh); %Exact profile for at1 to produce data.
    exact_at2 = ExactParameter(scaling_factor_at2,function_flag,time_mesh); %Exact profile for at2 to produce data.
    exact_k12 = ExactParameter(scaling_factor_k12,function_flag,time_mesh); %Exact profile for k12 to produce data.

    alpha = [exact_dm1; exact_dm2; exact_at1; exact_at2; exact_k12];

end

