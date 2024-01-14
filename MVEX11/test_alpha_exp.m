%%
clc, clf

t_min=0;t_max=20; m=5000; x_initial = [5*10^6; 10^3; 10^3];
time_mesh=linspace(t_min,t_max,m);
alpha = [10^-11 10^-12 10^-10 10^-12 10^-12]*100;
alpha_1=alpha_vec(alpha(1),alpha(2),alpha(3),alpha(4),alpha(5),time_mesh);
x = ForwardODE23s(alpha_1,time_mesh,x_initial); 
%x = ForwardNewton(alpha_1,time_mesh,x_initial);  
%x = ForwardODE45(alpha_1,time_mesh,x_initial);
t_plot = linspace(0,20,5000);


alpha_unknown=1;
alpha_exp=calculate_alpha_exp(alpha,alpha_unknown,x,t_min,t_max);

%Ger vanlig skala
%plot(t_plot(2:end-1),alpha_exp,LineWidth=1.5)
%hold on
%plot([0 20],[alpha(alpha_unknown) alpha(alpha_unknown)], 'r--')
%legend('Explicit beräkning','Sanna parametervärdet')

%Ger logaritmisk skala
plot(t_plot(2:end-1),log10(alpha_exp),LineWidth=1.5)
hold on
plot([0 20],[log10(alpha(alpha_unknown)) log10(alpha(alpha_unknown))], 'r--')
legend('Logaritmiska värdet av den explicita beräkningen','Sanna parametervärdet')

%% 
function alpha = alpha_vec(dm1,dm2,at1,at2,k12,time_mesh)
scaling_factor_dm1 = dm1;
scaling_factor_dm2 = dm2;
scaling_factor_at1 = at1;
scaling_factor_at2 = at2;
scaling_factor_k12 = k12;

function_flag = 0; % constant

exact_dm1 = ExactParameter(scaling_factor_dm1,function_flag,time_mesh); %Exact profile for dm1 to produce data.
exact_dm2 = ExactParameter(scaling_factor_dm2,function_flag,time_mesh); %Exact profile for dm2 to produce data.
exact_at1 = ExactParameter(scaling_factor_at1,function_flag,time_mesh); %Exact profile for at1 to produce data.
exact_at2 = ExactParameter(scaling_factor_at2,function_flag,time_mesh); %Exact profile for at2 to produce data.
exact_k12 = ExactParameter(scaling_factor_k12,function_flag,time_mesh); %Exact profile for k12 to produce data.

alpha = [exact_dm1; exact_dm2; exact_at1; exact_at2; exact_k12];

end
