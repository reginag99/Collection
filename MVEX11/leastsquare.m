clear,clc
% ----------------------------------------
%   Solution of least squares problem  min_x || Ax - y ||_2 
% ----------------------------------------

grad=5; % graden på polynomet 
points=200; %antalet diskreta punkter = ( rader i matrisen A)


% x = [1 2 3 4 5];
% y = [3 4 3.5 5 6];

x=zeros(1,points);
y=zeros(1,points);
V=[];
for i=1:1:points
  x = linspace(-10.0,10.0,points);
  %  exakt funktion
  y(i)= sin(pi*x(i)/5); %+ x(i)/5;
  % y=rand(1,i)*0.1;
  %y(i) = 1./(1+x(i).^2) + 0.1*randn(size(x(i)));
 
end


%skapar vandermode matrisen 
V = bsxfun(@power,x(:),0:grad);
%vandemorde = V 
% V*A=Y ekvation som ska lösas
% VL: V*V'*A dvs. VL=A
% LÖSNINGEN V*HL "(HL=A)"
% 

% beräknar högeledet 
HL=V'*y';

% beräknar vänsterledet
VL=V'*V;

%VL=zeros(degree+1);

% Lösning av normalekvationen med Cholesky faktorisering
%symmetri=issymmetric(VL)
if all(eig(VL) > 0) % vi kollar om matrisen är positivt definit eftersom chol kräver "symmetrisk positiv definita matriser" annars börjar den anta 
    VL = chol(VL, 'lower'); % chol faktoriserar matrisen lhs till en triangulär matris så att lhs=l'*l
    %felet kan kollas med:
    %norm(l*l' - lhs )
    HL = VL' \ (VL \ HL); 

    figure(1)
plot(x,y,'-- r', 'linewidth',2)
hold on

% beräknar approximation till det exakta polynomet med coefficienter c

polynom = V*HL;
plot(x,polynom,': b', 'linewidth',2)
hold off

str_xlabel = ['polynomgrad: ', num2str(grad)];

% Beräknar det relativa felet 
%  norm(approx. value - true value) / norm(true value)
%e1=norm(y'- approx)/norm(y')

error=norm(y'- polynom)/norm(y');
%fprintf('Relativt fel: %.2f%%', error);

legend('exakt ',str_xlabel);
title(['Relativt fel: ' num2str(error)])
xlabel('x')
else
    error('Det uppfylls inte att matrisen är positivt definit; en matris A sådan att x^(T) Ax > 0 för alla x');
end







