function out = betag_v2(T)
x = [260,280,300];
y = [2.573, 1.815, 1.327]*10^8;
p = polyfit(x,y,2); 
out = p(1)*T^2 + p(2)*T + p(3);   
end
%obs ej klar! 