function fun = Cond(T_surf,T_water,T_air,L,betag_v2,k,eps,r_outer,r_inner)

sigma = 5.67*10^-8;

A_surf = r_outer*2*pi*L;
A_top = r_inner^2*pi;

fun=(-k*2*pi*L*(T_surf-T_water)/log(r_outer/r_inner))-(h_cyl(T_surf,T_air,betag_v2,L,r_outer)*A_surf*(T_surf-T_air))-sigma*eps*A_surf*(T_surf^4 - T_air^4);

end