function out = Sc_air(T,DAB)
%function that solves for Schmidt number 
%DAB = mass diffusion rate
%T=Twater/Tair/2
out = my_air(T)/(rho_air(T) * DAB);