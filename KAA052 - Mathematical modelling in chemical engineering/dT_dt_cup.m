function dT_dt = dT_dt_cup(t,T)
%Variabler
T_water = T(1);
m = T(2);

%def av konstanter
k =17.3; %s 628 aluminium, for 100 C;
T_air = 20.6+273.15;
beta = betag_v2(T_air);
eps = 0.09;%s.352 Commericial sheet, aluminium, for 100 C
sigma = 5.67*10^-8;

L = 35*10^-3;
r_inner = sqrt(150*10^-6/(L*pi));  %for the mug
r_outer = r_inner + 4.99*10^-3;  %for the mug
D_AB = 2.634/101325;%p_water((T_water+T_air)/2); %ändraaaaaaaa plz
R = 8.3145;
M = (1.0079*2 + 16)*10^-3;


A_surf = r_outer*2*pi*L;
A_top = r_inner^2*pi;

L_water=125.81*10^-3/(rho_water(273.15+78.6)*2*r_inner*pi);

T0=[T_water-5];%initial guess for T_surf
options = optimoptions('fsolve','Display','none');
T_surf=fsolve(@(T_surf)Cond(T_surf,T_water,T_air,L,beta,k,eps,r_outer,r_inner),T0,options);
%eps = 0.032 + (0.35-0.032)/((3000+273.15)-(26 +273.15))*(T_surf-(26+273.15))

%k_c = h_top(T_water,T_air,betag_v2,r_inner)*(Sc_air((T_water + T_air)/2, D_AB)*pr_air(T_air,T_water))^(-2/3)/(cp_water(T_water)*rho_water((T_water+T_air)/2));
c_surf = p_water(T_air)*0.22/(R*T_air);
c_bulk = p_water((T_air+T_water)/2)/(R*(T_air+T_water)/2); %sida/kap 499/26, vi hade skrivit bulkkoncentrationen fel. Det är från vattenytan det evaporerar, därmed bör ångtrycket vid vattenytan användas som koncentration
k_c = kc(T_water,T_air,beta, r_inner,D_AB);

%derivator
dm_dt = k_c*(c_surf-c_bulk)*M*A_top;%/(1-c_surf);
rad = sigma*eps*A_surf*(T_surf^4 - T_air^4);
conv_heat = h_cyl(T_surf,T_air,beta,L,r_outer)*A_surf*(T_surf-T_air) +h_top(T_water,T_air,beta,r_inner)*A_top*(T_water-T_air);
conv_mass = dHvap_water(T_water)*dm_dt;

dTwater_dt =(-conv_mass-conv_heat -rad)/(rho_water(T_water)*cp_water(T_water)*r_inner*2*pi*L_water);  


%(-dTsurf_dt+h_surf*A_surf*(T_surf-T_air) + h_top*A_top*(T_water-T_air)+sigma*eps*A_surf*(T_surf^4 - T_air^4))/(rho_water(T_water)*cp_water(T_water)*r_inner*2*pi*L);
% q_svett = kc*(cas-cabulk)*dHvap *m
%rho cp dtwaterdt = konuktion + konvektion + radiation 
dT_dt = [dTwater_dt;dm_dt];
end