%%  Startvärden
clc; close all; clear
%%Startvärden på x och tidsvariabler
m = 500; % Antal observationer
obs_start = 2.1; obs_end = 7; %Intervall för observationer
time_final = 20;

time_mesh = linspace(0,time_final,m);
% x_initial = [x_T(0); x_M1(0); x_M2(0)]
% Startvärden som verkar passa fig 1a
x_initial = [5*10^6; 10^3; 10^3];

%% Variations in observations
%Nedanstående max och min förvardera parameter är respektives
%parameterintervall, givet i rapporten.
alpha_max = [10^-7 10^-8 10^-6 10^-8 10^-7];        
alpha_min = [10^-11 10^-12 10^-10 10^-12 10^-12];   
alpha_mid = 10.^((log10(alpha_min)+log10(alpha_max))/2);

% Konstanter att ändra för att plotta andra parametrar och grafer
alpha = alpha_mid';  % Värden på parametrar som ej varieras
big_var = 2;        % Hur många stora linjer
small_var = 5;     % Hur många små linjer per stor linje
alpha_chosen = 5;   % Vilken parameter ska varieras

alpha_chosen_variance = [alpha_min(alpha_chosen) alpha_max(alpha_chosen)];
variation=logspace(log10(alpha_chosen_variance(1)),log10(alpha_chosen_variance(end)),big_var);
variation_step=variation(2)/variation(1);

figure("Name", "Variation av parameter  " + alpha_chosen)
subplot(3,1,1)
xlabel('Dagar','FontSize',12,'FontWeight','bold')
ylabel('X_T, [celler]','FontSize',12,'FontWeight','bold')
hold on
subplot(3,1,2)
xlabel('Dagar','FontSize',12,'FontWeight','bold')
ylabel('X_{M1}, [celler]','FontSize',12,'FontWeight','bold')
hold on
subplot(3,1,3)
xlabel('Dagar','FontSize',12,'FontWeight','bold')
ylabel('X_{M2}, [celler]','FontSize',12,'FontWeight','bold')
hold on



dark_green = [30,120,130]/255;
dark_orange = [112,81,28]/255;
dark_purple = [101,120,163]/255;

light_green = [122,214,185]/255;
light_orange = [232,131,78]/255;
light_purple = [151,190,203]/255;

greenGRADIENTlight = @(i,N) light_green + (dark_green-light_green)*((i-1)/(N-1));
greenGRADIENTdark  = @(i,N) dark_green - (dark_green-light_green)*((i-1)/(N-1));
orangeGRADIENTlight = @(i,N) light_orange + (dark_orange-light_orange)*((i-1)/(N-1));
orangeGRADIENTdark  = @(i,N) dark_orange - (dark_orange-light_orange)*((i-1)/(N-1));
purpGRADIENTlight = @(i,N) light_purple + (dark_purple-light_purple)*((i-1)/(N-1));
purpGRADIENTdark  = @(i,N) dark_purple - (dark_purple-light_purple)*((i-1)/(N-1));

N_small=small_var*big_var-1;
N_big=big_var;
alpha_read=[];

b=logspace(log10(alpha_chosen_variance(1)),log10(alpha_chosen_variance(end)),big_var*(small_var+1));

for i=1:length(b)
    alpha(alpha_chosen,:)=b(i);
    alpha=alpha_vec(alpha(1),alpha(2),alpha(3),alpha(4),alpha(5),time_mesh);
    F45 = ForwardODE45(alpha,time_mesh,x_initial);

    if i==1
        subplot(3,1,1)
        plot(time_mesh,F45(1,:),'color',orangeGRADIENTlight(i,length(b)),'linewidth',2);
        subplot(3,1,2)
        plot(time_mesh,F45(2,:),'color',greenGRADIENTlight(i,length(b)),'linewidth',2);
        subplot(3,1,3)
        plot(time_mesh,F45(3,:),'color',purpGRADIENTlight(i,length(b)),'linewidth',2);

    elseif i==length(b)

        subplot(3,1,1)
        plot(time_mesh,F45(1,:),'color',orangeGRADIENTlight(i,length(b)),'linewidth',2);
        subplot(3,1,2)
        plot(time_mesh,F45(2,:),'color',greenGRADIENTlight(i,length(b)),'linewidth',2);
        subplot(3,1,3)
        plot(time_mesh,F45(3,:),'color',purpGRADIENTlight(i,length(b)),'linewidth',2);
        F=F45

    else
    orangeGRADIENTlight(i,N_small)
    subplot(3,1,1)
    plot(time_mesh,F45(1,:),'color',orangeGRADIENTlight(i,length(b)),'linestyle','--','linewidth',1);        
    subplot(3,1,2)
    plot(time_mesh,F45(2,:),'color',greenGRADIENTlight(i,length(b)),'linestyle','--','linewidth',1);
    subplot(3,1,3)
    plot(time_mesh,F45(3,:),'color',purpGRADIENTlight(i,length(b)),'linestyle','--','linewidth',1);


    end

end


% 
% for i_big_var=1:big_var
%     alpha(alpha_chosen,:)=variation(i_big_var);
%     alpha_read=[alpha_read; alpha(alpha_chosen,1)];
%     alpha=alpha_vec(alpha(1),alpha(2),alpha(3),alpha(4),alpha(5),time_mesh);
%     F45 = ForwardODE45(alpha,time_mesh,x_initial);
% 
%     if i_big_var == 1
%         for i_small_var=1:small_var
%             line_width = 1;
%             alpha_plus_var = alpha;
%             (variation_step/2*i_small_var/small_var)
%             alpha_plus_var(alpha_chosen,:) = alpha_plus_var(alpha_chosen,:)*(variation_step/2*i_small_var/small_var);
%             alpha_read=[alpha_read; alpha_plus_var(alpha_chosen,1)];
%             small_plus_F45 = ForwardODE45(alpha_plus_var,time_mesh,x_initial);
% 
%             subplot(3,1,1)
%             plot(time_mesh,small_plus_F45(1,:),'color',orangeGRADIENTlight(i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,2)
%             plot(time_mesh,small_plus_F45(2,:),'color',greenGRADIENTlight(i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,3)
%             plot(time_mesh,small_plus_F45(3,:),'color',purpGRADIENTlight(i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             a = alpha(:,1);
%         end
%     elseif i_big_var == length(variation)
%         for i_small_var=1:small_var-1
%             line_width = 1;
%             alpha_minus_var=alpha;
%             alpha_minus_var(alpha_chosen,:) = alpha_minus_var(alpha_chosen,:)*(0.5 + (i_small_var-1)/small_var/2);
%             alpha_read=[alpha_read; alpha_minus_var(alpha_chosen,1)];
%             small_minus_F45 = ForwardODE45(alpha_minus_var,time_mesh,x_initial);
% 
%             subplot(3,1,1)
%             plot(time_mesh,small_minus_F45(1,:),'color',orangeGRADIENTdark(small_var-i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,2)
%             plot(time_mesh,small_minus_F45(2,:),'color',greenGRADIENTdark(small_var-i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,3)
%             plot(time_mesh,small_minus_F45(3,:),'color',purpGRADIENTdark(small_var-i_small_var,N_small),'linestyle','--','linewidth',line_width);
%         end
%     else
%         for i_small_var=1:small_var
%             line_width = 1;
% 
%             alpha_plus_var = alpha;
%             alpha_plus_var(alpha_chosen,:) = alpha_plus_var(alpha_chosen,:)*(variation_step/2*i_small_var/small_var);
%             alpha_read=[alpha_read; alpha_plus_var(alpha_chosen,1)];
%             small_plus_F45 = ForwardODE45(alpha_plus_var,time_mesh,x_initial);
% 
%             subplot(3,1,1)
%             plot(time_mesh,small_plus_F45(1,:),'color',orangeGRADIENTlight(i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,2)
%             plot(time_mesh,small_plus_F45(2,:),'color',greenGRADIENTlight(i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,3)
%             plot(time_mesh,small_plus_F45(3,:),'color',purpGRADIENTlight(i_small_var,N_small),'linestyle','--','linewidth',line_width);
%         end
%         for i_small_var=1:small_var-1
%             line_width = 1;
% 
%             alpha_minus_var=alpha;
%             alpha_minus_var(alpha_chosen,:) = alpha_minus_var(alpha_chosen,:)*(0.5 + (i_small_var-1)/small_var/2);
%             alpha_read=[alpha_read; alpha_minus_var(alpha_chosen,1)];
%             small_minus_F45 = ForwardODE45(alpha_minus_var,time_mesh,x_initial);
% 
%             subplot(3,1,1)
%             plot(time_mesh,small_minus_F45(1,:),'color',orangeGRADIENTdark(small_var-i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,2)
%             plot(time_mesh,small_minus_F45(2,:),'color',greenGRADIENTdark(small_var-i_small_var,N_small),'linestyle','--','linewidth',line_width);
%             subplot(3,1,3)
%             plot(time_mesh,small_minus_F45(3,:),'color',purpGRADIENTdark(small_var-i_small_var,N_small),'linestyle','--','linewidth',line_width);
% 
%         end
%     end
%     subplot(3,1,1)
%     plot(time_mesh,F45(1,:),'color',orangeGRADIENTlight(i_big_var,N_big),'linewidth',2);
%     subplot(3,1,2)
%     plot(time_mesh,F45(2,:),'color',greenGRADIENTlight(i_big_var,N_big),'linewidth',2)
%     subplot(3,1,3)
%     plot(time_mesh,F45(3,:),'color',purpGRADIENTlight(i_big_var,N_big),'linewidth',2)
% end
alpha_read=sort(alpha_read);

%% Inner functions

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
