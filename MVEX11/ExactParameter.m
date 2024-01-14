function exact_par = ExactParameter(scaling,flag,time_mesh)
    %Input: 1. scaling factor
    %2. function flag (0. const. 1. +linear 2. -linear 3. sin 4. exp(-x))
    %time = 0;
    s = zeros(1,length(time_mesh));
    s = 3*time_mesh/time_mesh(end);
    if flag == 1
        exact_par = scaling*s/3;
    elseif flag == 2
        exact_par = scaling*(1-s/3);
    elseif flag == 3
        exact_par = scaling*sin(s);
    elseif flag == 4
        exact_par = scaling*exp(-s);
    else
        exact_par = scaling*ones(1,length(time_mesh));
    end

    
%    figure
%	plot(time_mesh,exact_par,'b --s','LineWidth',3);
%legend(' linear 1')
% title('Test function in time')
 
end