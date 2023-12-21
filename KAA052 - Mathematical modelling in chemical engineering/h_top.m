function out = h_top(T_water,T_air, betag_v2, r_inner)%D = diameter of the beaker
%calculate heat transfer coefficient. Referenses are to the book
%"fundamentals of momentum, heat and masstransfer"

deltaT = T_water - T_air;
D = r_inner *2;
Gr = betag_v2 *D^3*deltaT;%s.306
Ra = Gr * pr_air(T_air,T_water); % eq 19-3

if (10^4<Ra) && (Ra<2*10^7) %s.302
    C = 0.54;
    x = 1/4;
elseif (2*10^7<Ra) && (Ra<3*10^(19))
    C = 0.14;
    x = 1/3;
end

out = C*Ra^x*k_air((T_water+T_air)/2)/D;

end