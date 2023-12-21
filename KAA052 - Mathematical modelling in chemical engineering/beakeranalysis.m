%% Plot of temperature at h=H/4 for different radius
r0 = [79.3 78.0 76.7 74.9 73.80];
t0=[1 2 3 4 5];
r2=[72.6 71.8 70.7 69.9 68.6];
t2=[6 7 8 9 10];
rr=[67.6 66.9 65.8 65.0 64.4];
tr=[11 12 13 14 15];
figure
subplot(2,2,1);
plot(t0,r0,'r'); hold on;
plot(t2,r2,'g'); hold on;
plot(tr,rr,'b'); hold on;
legend('r=0','r=R/2','r=R');
xlabel('Time (min)');
ylabel('Temperature (C)');
hold off;
title('Plot of temperature at h=H/4 for different radius');



%% Plot of temperature at h=H/2 for different radius
r0 = [63.9 63.4 63.0 62.4 61.9];
t0=[16 17 18 19 20];
r2=[ 61.5 61.0 60.5 60.3 59.6];
t2=[ 21 22 23 24 25];
rr=[ 59.1 58.5 57.8 57.3 56.5];
tr= [26 27 28 29 30];
subplot(2,2,2);
plot(t0,r0,'r'); hold on;
plot(t2,r2,'g'); hold on;
plot(tr,rr,'b'); hold on;
legend('r=0','r=R/2','r=R');
xlabel('Time (min)');
ylabel('Temperature (C)');
hold off;
title('Plot of temperature at h=H/2 for different radius');
%% Plot of temperature at h=H for different radius
r0 = [56.1 55.4 55.0 54.5 53.9];
t0=[31 32 33 34 35];
r2=[52.5 52.2 52.0 51.6 51.2];
t2=[36 37 38 39 40];
rr=[50.7 50.4 49.3 49.1 48.6];
tr=[41 42 43 44 45];
subplot(2,2,3);
plot(t0,r0,'r'); hold on;
plot(t2,r2,'g'); hold on;
plot(tr,rr,'b'); hold on;
legend('r=0','r=R/2','r=R');
xlabel('Time (min)');
ylabel('Temperature (C)');
hold off;
title('Plot of temperature at h=H for different radius');



%% Plot of temperatures for H/4, H/2 och H (from r=0 to r=R)
figure
rh4 = [79.3 78.0 76.7 74.9 73.80 72.6 71.8 70.7 69.9 68.6 67.6 66.9 65.8 65.0 64.4];
th4=[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15];
plot(th4,rh4,'r'); hold on;

rh2 = [63.9 63.4 63.0 62.4 61.9 61.5 61.0 60.5 60.3 59.6 59.1 58.5 57.8 57.3 56.5];
th2=[16 17 18 19 20 21 22 23 24 25 26 27 28 29 30];
plot(th2,rh2,'b'); hold on;

rh = [56.1 55.4 55.0 54.5 53.9 52.5 52.2 52.0 51.6 51.2 50.7 50.4 49.3 49.1 48.6];
th=[31 32 33 34 35 36 37 38 39 40 41 42 43 44 45];
plot(th,rh,'g'); hold on;
xlabel('Time (min)');
ylabel('Temperature (C)');
legend('H/4','H/2','H');
%Är ungefär samma radiella gradient för H/2 och H, vilket är hälften så
%stor som den radiella gradienten i H/4. Alla radiella gradienter är
%konstanta mellan r=0 och r=R. 


%% Plot of mass at all radiuses for h= H/4, H/2, H
figure
mh4 = [739.37 738.60 737.25 736.05 735.20 734.40 733.81 733.09 732.36 731.62 731.07 730.38 729.88 729.23 728.61];
th4=[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15];
plot(th4,mh4,'r'); hold on;

mh2 = [727.99 727.77 727.54 727.10 726.84 726.58 726.24 726.01 725.84 725.48 724.90 724.65 724.20 724.08 723.48];
th2=[16 17 18 19 20 21 22 23 24 25 26 27 28 29 30];
plot(th2,mh2,'b'); hold on;

mh = [722.86 722.65 722.32 722.02 721.59 720.80 720.62 720.54 720.34 720.09 719.77 719.49 719.27 719.05 718.89];
th=[31 32 33 34 35 36 37 38 39 40 41 42 43 44 45];
plot(th,mh,'g'); hold on;
xlabel('Time (min)');
ylabel('Mass (g)');
legend('H/4','H/2','H');
%Är ungefär samma radiella gradient för H/2 och H, vilket är hälften så
%stor som den radiella gradienten i H/4. Alla radiella gradienter är
%konstanta mellan r=0 och r=R. 

