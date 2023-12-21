function out = kc(T_water,T_air,betag_v2,r_inner,DAB)
%mass transfer coefficient for convective mass transfer between water and
%air 
%cp, rho will be at Twater+Tair/2
T = (T_water+T_air)/2; 
h = h_top(T_water,T_air,betag_v2,r_inner);
Sc = Sc_air(T,DAB);
Pr = pr_air(T_air,T_water);
Cp_rho = cp_air(T)*rho_air(T);

out = h*(Pr/Sc)^(2/3)/Cp_rho;