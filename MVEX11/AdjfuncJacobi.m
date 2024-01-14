function lambda = AdjfuncJacobi(x,alpha)
    % alpha = (d_m1, d_m2, a_t1, a_t2, k_12)
     r = 0.93;
     beta_T = 3*10^9;
     d_m1 = alpha(1);
     d_m2 = alpha(2);
     a_t1 = alpha(3);
     a_t2 = alpha(4);
     beta_M = 9*10^8;
     sigma_m1 = 0.173;
     sigma_m2 = 0.173;
     k_12 = alpha(5);
     
     x_T = x(1); x_M1 = x(2); x_M2 = x(3);   
     lambda = zeros(3,3);

    lambda(1,1) = -r*(1-2*x_T/beta_T) + d_m1*x_M1 - d_m2*x_M2;
    lambda(1,2) = -a_t1*x_M1*(1-(x_M1+x_M2)/beta_M) + k_12*x_M1;
    lambda(1,3) = -a_t2*x_M2*(1-(x_M1+x_M2)/beta_M) - k_12*x_M1;
    lambda(2,1) = d_m1*x_T;
    lambda(2,2) = -a_t1*x_T*(1-(2*x_M1+x_M2)/beta_M) + sigma_m1 + k_12*x_T;
    lambda(2,3) = a_t2*x_T*x_M2/beta_M - k_12*x_T;
    lambda(3,1) = -d_m2*x_T;
    lambda(3,2) = a_t1*x_T*x_M1/beta_M;
    lambda(3,3) = sigma_m2 - a_t2*x_T*(1-(x_M1+2*x_M2)/beta_M);
    
end