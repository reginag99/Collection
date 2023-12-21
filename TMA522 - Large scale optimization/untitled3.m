%t = 0;
nOfRelaxedConstraints = 2 * dimX * dimY;
u = zeros(1, nOfRelaxedConstraints);
theta = 2;
thetaCount = 0;
nrOfIterations = 1000;
gammaT = [];
k_erg = 2;%decide on convexity weight rule
ht = [];%Måste denna initieras eller är det onödigt?
x_erg = zeros(1,nOfRelaxedConstraints);%fel i dim
for iteration = 1:nrOfIterations
    [x, ht] = SolveLagrangeanSubProblem(u); %solving the lagrangian subproblem at u_t

    h = max(h,ht);%keep the best ht(u_t) aka upper boundery
  
    gammaT = CalculateSubGradientDirection(x, n, k);
    alpha = theta*(k-ht)/gammaT^2;
    %theta är en step length parameter, inget mr speciellt
    if u == 0
        u = max(0,u+alpha*max(0,gammaT));
    elseif u > 0
        u = max(0,u+alpha*0,gammaT);
    end
    u = max(0,u+alpha*max(0,gammaT));
    x_erg(1) = x()
    for s = 0:iteration-1
        x_erg_u = x_erg_u + (s+1)^k;
        x_erg_l = x_erg_l + (s+1)^k;
        x_erg(1,iteration) = x_erg_u/x_erg_l*x_erg(1,iteration-1) + t^k/x_erg_l*x_erg(1,iteration-2);
    end
    %t = t + 1;
    if mod(iteration, 10) == 0
        theta = theta * 0.95;
    end
end

%Lagrangian heuristic
