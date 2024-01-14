function alpha_exp=mod_calculate_alpha_exp_del1(alpha,alpha_unknown,x,t_min,t_max)

x_T=x(1,:);
x_M1=x(2,:);
x_M2=x(3,:);
m=length(x_T) - 1; %ändrade till -1 då jag tycker steglängden borde bli det
time_step=(t_max-t_min)/m;
alpha_exp=zeros(m-1,1); %ändrade från m-2 till m-1

for k=2:m
    x_k=x(:,k);
    
        dxT_dt=(x_T(k+1)-x_T(k-1))/(2*time_step);
        dxM1_dt=(x_M1(k+1)-x_M1(k-1))/(2*time_step);
        dxM2_dt=(x_M2(k+1)-x_M2(k-1))/(2*time_step);

        dx_dt=[dxT_dt dxM1_dt dxM2_dt];
      
        alpha_exp(k-1)=calc_explicit_FE(alpha,alpha_unknown,x_k,dx_dt);
end 
end





function alpha_exp_k_FE=calc_explicit_FE(alpha,alpha_unknown,x,dx_dt);
dm1=alpha(1);
dm2=alpha(2);
at1=alpha(3);
at2=alpha(4);
k12=alpha(5);
r=0.93;
deltaM1=0.173;
deltaM2=0.173;
betaT=3*10^9;
betaM=9*10^8;

xT=x(1);
xM1=x(2);
xM2=x(3);

dxT_dt=dx_dt(1);
dxM1_dt=dx_dt(2); 
dxM2_dt=dx_dt(3);

if alpha_unknown==1
    alpha_exp_k_FE = r*(1-xT/betaT)/xM1 - dxT_dt/(xM1*xT) ; %+ dm2*xM2/xM1
elseif alpha_unknown==2
    alpha_exp_k_FE = dxT_dt/(xM2*xT) - r*(1-xT/betaT)/xM2 ; %+ dm1*xM1/xM2
elseif alpha_unknown==3
    alpha_exp_k_FE = dxM1_dt/(xT*xM1*(1 - (xM2+xM1)/betaM)) +deltaM1/(xT*(1-(xM1+xM2)/betaM)); %  + k12/(1-(xM1+xM2)/betaM)
elseif alpha_unknown==4
    alpha_exp_k_FE = dxM2_dt/(xT*xM1*(1 - (xM2+xM1)/betaM)) + deltaM2/(xT*(1 - (xM2+xM1)/betaM));%  - k12/(1 - (xM2+xM1)/betaM)
else
    alpha_exp_k_FE = -dxM1_dt/(xM1*xT) - deltaM1/xT ; %+ at1*(1-(xM1 + xM2)/betaM)
end
end
