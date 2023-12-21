clear; clc; clf; close  all
T_water0 = 90+273.15;
m0 = 150*10^-3;


T0 = [T_water0,m0];%[T_surf0, T_water0,(T_surf0-T_water0)/(r_outer-r_inner)];%T_surf, T_water
t_span = linspace(0,50000);
t_h = t_span(end)/3600



%T-plots
[t,T] = ode45(@dT_dt_cup,t_span,T0);
subplot(2,1,1)
plot(t,T(:,1), LineWidth=1); hold on;
temp=T(:,1);
mass=T(:,2);
yline(273.15+50, 'r--', LineWidth=1) 
legend('Model Twater (K)','323.15 K'); 
xlabel('Time, [s]','FontSize',12,'FontWeight','bold')
ylabel('Temperature, [K]','FontSize',12,'FontWeight','bold')

%mass plots
subplot(2,1,2)
plot(t,T(:,2),LineWidth=1); hold on;
legend('Model mass(g)');
xlabel('Time, [s]','FontSize',12,'FontWeight','bold')
ylabel('Mass, [g]','FontSize',12,'FontWeight','bold')


fontsize(16,"points")
