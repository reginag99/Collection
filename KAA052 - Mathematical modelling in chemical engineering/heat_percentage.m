clear; clc; close  all
S=readtable("small_beaker_3.txt");
T_water0 = S{1,2}+273.15;
m0 = S{1,3}*10^-3;
r_outer = 70*10^-3;
r_inner = 63*10^-3;

T0 = [T_water0,m0];%[T_surf0, T_water0,(T_surf0-T_water0)/(r_outer-r_inner)];%T_surf, T_water
t_span = S{:,1};

[t, T] = ode45(@(t,T)dT_dt_G(t,T),t_span,T0);

T_water=T(:,1);
m=T(:,2);

%def av konstanter
k = 0.9;
T_air=20.6+273.15;
L=95*10^-3;
beta = betag_v2(T_air);

sigma = 5.67*10^-8;
eps = 0.95;
r_outer = 70*10^-3;
r_inner = 63*10^-3;
D_AB = 2.634/101325;%p_water((T_water+T_air)/2); %ändraaaaaaaa plz
R = 8.3145;
M = (1.0079*2 + 16)*10^-3;


A_surf = r_outer*2*pi.*L;
A_top = r_inner^2*pi;

L_water=125.81*10^-3/(rho_water(273.15+78.6)*2*r_inner*pi);

Tsurf0=[T_water-5];%initial guess for T_surf
options = optimoptions('fsolve','Display','none');
%T_surf=fsolve(@(T_surf)Cond(T_surf,T_water,T_air,L,beta),T0,options);


heat = [];
%derivator
for i=1:length(t)
    options = optimoptions('fsolve','Display','none');
    T_surf=fsolve(@(T_surf)Cond(T_surf,T_water(i),T_air,L,beta,k,eps,r_outer,r_inner),Tsurf0(i),options);
rad = sigma*eps.*A_surf*(T_surf^4 - T_air^4);
conv_heat_cyl = h_cyl(T_surf,T_air,beta,L,r_outer).*A_surf*(T_surf-T_air);
conv_heat_top = h_top(T_water(i),T_air,beta,r_inner)*A_top*(T_water(i)-T_air);
c_surf = p_water(T_air)*0.22/(R*T_air);
c_bulk = p_water((T_air+T_water(i))/2)/(R*(T_air+T_water(i))/2);
k_c = kc(T_water(i),T_air,beta, r_inner,D_AB);
dm_dt = k_c*(c_surf-c_bulk)*M*A_top;
conv_mass = -dHvap_water(T_water(i))*dm_dt;
heat = [heat; rad(end,:) conv_heat_cyl(end,:) conv_heat_top(end,:) conv_mass(end,:)];
end

heat;
format long;
heat_tot=(heat(:,1)+heat(:,2)+heat(:,3)+heat(:,4));
conduction = (heat(:,1)+heat(:,2))./heat_tot; % (%)
convection_top = heat(:,3)./heat_tot; % (%)
convection_walls = heat(:,2)./heat_tot; % (%)
radiation = heat(:,1)./ heat_tot; % (%)
evaporation = heat(:,4)./heat_tot;

subplot(3,2,1)
plot(t,radiation,'g','LineWidth',1);
xlabel('Time (s)');
ylabel('q_r_a_d/q_t_o_t (%)');
legend('Radiation');

subplot(3,2,2);
plot(t,convection_walls,'r','LineWidth',1);
xlabel('Time (s)');
ylabel('q_w_a_l_l_s/q_t_o_t (%)');
legend('Convection beaker walls');

subplot(3,2,3);
plot(t,convection_top,'b','LineWidth',1);
xlabel('Time (s)');
ylabel('q_t_o_p/q_t_o_t (%)');
legend('Convection beaker top');

subplot(3,2,4);
plot(t,conduction,'m','LineWidth',1);
xlabel('Time (s)');
ylabel('q_c_o_n_d/q_t_o_t (%)');
legend('Conduction');

subplot(3,2,5);
plot(t,evaporation,'k','LineWidth',1);
xlabel('Time (s)');
ylabel('q_e_v_a_p/q_t_o_t (%)');
legend('Evaporation');