clear; clc; clf; close  all
T_mugg0 = 90+273.15;


r_outer = 41*10^-3;
r_inner = 35*10^-3;


T0 = [T_mugg0];%[T_surf0, T_water0,(T_surf0-T_water0)/(r_outer-r_inner)];%T_surf, T_water
t_span = [0 10000];


%T-plots
[t,T] = ode45(@dT_dt_mugg,t_span,T0);

plot(t/3600,(T(:,1)-273.15)); hold on;
yline(50,'r-');

legend('Model Tcup (^oC)'); ylabel('Temperature of cup (^oC)'); xlabel('Time (h)')

