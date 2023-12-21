function dT_dt = dT_dt_RG2(t,T,It_opt,m0)
%Variables
select = It_opt(1);%Constant that will change
I = It_opt(2);%number of different values of constant
iter = It_opt(3);%Which iteration of the constant
T_water = T(1);
m = T(2);

%def constants, baseline
k_glas = 0.9;
k = k_glas;
L=95*10^-3;
T_air = 20.6+273.15;
beta = betag_v2(T_air);

sigma = 5.67*10^-8;
eps = 0.95;
r_outer = 70*10^-3;
r_inner = 63*10^-3;
D_AB = 2.634/101325;
R = 8.3145;
M = (1.0079*2 + 16)*10^-3;

Tair_min = 273.15 + 15;
Tair_max = 273.15 + 25;
dT = (Tair_max-Tair_min)/I;

rinner_min = 50*10^-3/2;
rinner_max = 70*10^-3/2;
drinner = (rinner_max-rinner_min)/I;

router_min = r_inner/2-5*10^-3;
router_max = 70*10^-3/2;
drouter = (router_max-router_min)/I;

L_min = 80*10^-3;
L_max = 120*10^-3;
dL = (L_max - L_min)/I;

eps_min = 0.5;
eps_max = 1;
deps = (eps_max - eps_min)/I;

k_max = 1;
k_min = 0.1;
dk = (k_max-k_min)/I;

if select == 1
    T_air = Tair_min + dT*iter;
elseif select == 2
    r_inner = rinner_min + drinner*iter;
elseif select == 3
    r_outer = router_min + drouter*iter;
elseif select == 4
    L = L_min + dL*iter;
elseif select == 5
    eps = eps_min + iter*deps;
elseif select == 6
    k = k_min + iter*dk;
end
    

beta = betag_v2(T_air);
A_surf = r_outer*2*pi*L;
A_top = r_inner^2*pi;
L_water=m0/(rho_water(273.15+78.6)*2*r_inner*pi);

T0=[T_water-5];%initial guess for T_surf
options = optimoptions('fsolve','Display','none');
T_surf=fsolve(@(T_surf)Cond(T_surf,T_water,T_air,L,beta,k,eps,r_outer,r_inner),T0,options);

c_surf = p_water(T_air)*0.22/(R*T_air);
c_bulk = p_water((T_air+T_water)/2)/(R*(T_air+T_water)/2); %sida/kap 499/26, 
k_c = kc(T_water,T_air,beta, r_inner,D_AB);

%derivator
dm_dt = k_c*(c_surf-c_bulk)*M*A_top;%/(1-c_surf);
rad = sigma*eps*A_surf*(T_surf^4 - T_air^4);
conv_heat = h_cyl(T_surf,T_air,beta,L,r_outer)*A_surf*(T_surf-T_air) +h_top(T_water,T_air,beta,r_inner)*A_top*(T_water-T_air);
conv_mass = dHvap_water(T_water)*dm_dt;

dTwater_dt =(-conv_mass-conv_heat -rad)/(rho_water(T_water)*cp_water(T_water)*r_inner*2*pi*L_water);  

dT_dt = [dTwater_dt;dm_dt];
end
