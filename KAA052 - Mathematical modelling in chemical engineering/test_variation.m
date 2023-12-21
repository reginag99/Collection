%The plot shows the heat and mass transfer for different values of the
%constants. The constants that can vary in the ones mentioned in
%'selectiontype', 'Select' refers to the constant you want to vary and 'I'
%is the number of different values you want to include in the plot. 
%The max and min values for each constant that will be tested can be found 
% in the file 'dT_dt_RG2.m' The values of the constant is illustrated by 
% a colourgradient. 

%Lighter colour = low values of constant
%Darker colour = high values of constant

clear; clc; clf; close  all
S=readtable("small_beaker_3.txt"); %change is comparing with another set of data
T_water0 = S{1,2}+273.15;
m0 = S{1,3}*10^-3;

T0 = [T_water0,m0]; %inital values for the ode
t_span =linspace(0,2000);% S{:,1};
selctiontype = [{'T_{air}'}, {'r_{inner}'}, {'r_{outer}'}, {'L'}, {'epsilon'},{'k'}];%constants that can change. Choose one of them with 'Select'
Select = 4; %The constant that will vary. 1 = T_air, 2 = r_inner ect
I = 7; %Number of different values of constant you want to include in the plot. OBS! If I =m, the plot will have m +1 iterations and lines as it will also plot iter = 0.

figure("Name", "Variatin of constant  " + selctiontype(Select))
subplot(2,1,1)
xlabel('Time, [s]','FontSize',12,'FontWeight','bold')
ylabel('Temperature, [K]','FontSize',12,'FontWeight','bold')
hold on
subplot(2,1,2)
xlabel('Time, [s]','FontSize',12,'FontWeight','bold')
ylabel('Mass, [g]','FontSize',12,'FontWeight','bold')
hold on

Green_light = [60,200,160]/255;
Green_dark = [10,130,90]/255;
Orange_light = [255,130,30]/255;
Orange_dark = [180,60,0]/255;

for iter = 0:I
    Green_grad = @(iter) Green_light + (Green_dark - Green_light)/I*iter;
    Orange_grad = @(iter) Orange_light + (Orange_dark - Orange_light)/I*iter;

    It_opt = [Select,I,iter];
    [t,T] = ode45(@(t,T) dT_dt_RG2(t,T,It_opt,m0),t_span,T0);

    %Temp plots
    subplot(2,1,1)
    plot(t,T(:,1),'Color',Green_grad(iter),'linewidth',1); hold on;
    title(['Variation analysis for ', selctiontype{Select}, ', heat transfer'])
   % yline(273.15+50, 'r--', LineWidth=1)  
    %mass plots
    subplot(2,1,2)
    plot(t,T(:,2),'Color',Orange_grad(iter),'linewidth',1); hold on;
    title(['Variation analysis for ' selctiontype{Select} ', mass transfer'])
end

