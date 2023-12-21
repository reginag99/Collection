function out = h_cyl(T_surf,T_air, betag_v2, L,r_outer)%D = diameter of the beaker
%calculate heat transfer coefficient. Referenses are to the book
%"fundamentals of momentum, heat and masstransfer"

deltaT = T_surf - T_air;
Gr = betag_v2 *L^3*deltaT;%s.306
Ra = Gr * pr_air(T_air,T_surf); % eq 19-3


%if 2*r_outer > 35/Gr^(1/4)
    Nu = 0.68 + 0.67*Ra^(1/4)/(1+(0.492/pr_air(T_air,T_surf)));
    out = Nu*k_air((T_surf+T_air)/2)/L;
%else
 %   error('Nu can not be calculated as a vertical cylinder')
%%end

end