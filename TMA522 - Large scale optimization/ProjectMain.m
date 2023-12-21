nOfRelaxedConstraints = 2 * dimX * dimY;
u = zeros(1, nOfRelaxedConstraints) + 1/nOfRelaxedConstraints;
theta = 2;
nrOfIterations = 1000;
gammaT = [];
h_bestUpperBound = Inf;
h_bestLowerBound = 0;
bestnl = [];
bestContactPairs = 0;
ReusedNodes = [];
primalSolution = [];
dualSolution = [];
    
for iteration = 1:nrOfIterations
    disp(iteration)
    [solution, contactPairs, nl, feasCom, ReusedNodes] = GetFeasiblesolution(dimX, dimY, u, k, com, ReusedNodes);
    [x, ht, newnl, okcom] = SolveLagrangeanSubProblem(dimX, dimY, u, k, com);
    if contactPairs > bestContactPairs
        bestnl = nl;
        bestContactPairs = contactPairs;
        bestcom = feasCom;
    end
    
    
    h_bestUpperBound = min(h_bestUpperBound, ht); %keep the best ht(u_t) aka upper boundery
    gammaT = CalculateSubGradientDirection(x, k, dimX, dimY);
    alpha = theta*(ht - h_bestLowerBound)/(norm(max(0,gammaT), 2)^2);
    
        
    if u == 0
        u = max(0,u+alpha*min(0,gammaT));
    elseif u > 0
        u = max(0,u+alpha*gammaT);
    end

    
    if mod(iteration, 10) == 0
        theta = theta * 0.95;
    end

    
    dualSolution = [dualSolution, h_bestUpperBound];
    primalSolution = [primalSolution,contactPairs];
    
    %break if we have a solution with desired number of contact pairs
    if bestContactPairs == k
        disp("A solution has been found")
        break
    end
end
figure
visagrid(dimX, dimY, bestnl, bestcom, u, 25);
iteration = [1:nrOfIterations];

figure
plot(iteration,dualSolution,'r', iteration, primalSolution, 'b')


