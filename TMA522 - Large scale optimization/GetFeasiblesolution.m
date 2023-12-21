function [solutions, nrContactPairs, nl, okcom,ReusedNodes] = GetFeasiblesolution(dimX, dimY, u, k, com,ReusedNodes)
    nrNodes = dimX*dimY*2;
    linkCosts = u';
    %linkCosts = zeros(dimX*dimY*2, 1);
    penalty = 10000;
    %newSolutions = gsp(dimX, dimY, u, k, com);   
    %isDuplicates = length(newSolutions) ~= length(unique(newSolutions));
    solutions = [];
    
    
    for cP = 1:k
        % set penalty on each startnode
        linkCosts(com(cP, 1)) = 100000;
        % set penalty on each terminalnode
        linkCosts(com(cP, 2)) = 100000;
    end
    
    u(ReusedNodes) = u(ReusedNodes) + 1000;
    
    %rand com
    com = com(randperm(size(com, 1)), :);
    
    for contactPair = 1:k
        individualSolution = gsp(dimX, dimY, linkCosts, 1, com(contactPair, :));   % gives us index of passed nodes
        
        % check that we have not already passed the solution
        if linkCosts(individualSolution(2:end-1)) < 10000
            solutions(contactPair).path = individualSolution;
            linkCosts(individualSolution) = linkCosts(individualSolution) + penalty;  
        else
            % does it cross more than one path
            nrPathsCrossed = 0;
            %pathIndex = 0;
            for previousSolutions = 1:length(solutions)
                if ~isempty(intersect(individualSolution, solutions(previousSolutions).path))
                    nrPathsCrossed = nrPathsCrossed + 1;
                    oldPathIndex = previousSolutions;
                end
            end
            
            % compare cost of paths with the path it crossed if it is 1
            if nrPathsCrossed == 1
                newPathCost = sum(u(individualSolution));
                oldPathCost = sum(u(solutions(oldPathIndex).path));
                
                if newPathCost < oldPathCost
                    % remove penalty from old path nodes
                    linkCosts(solutions(oldPathIndex).path(2:end-1)) = 0;
                    doubleUsedNode = intersect(solutions(oldPathIndex).path, individualSolution);
                    linkCosts(doubleUsedNode) = penalty;
                    
                    
                    solutions(oldPathIndex).path = NaN;
                    % add penalty to new path nodes
                    solutions(contactPair).path = individualSolution;
                    linkCosts(individualSolution) = linkCosts(individualSolution) + penalty;
                    
                    % try to find new path for removed contact pair
                    test = gsp(dimX, dimY, linkCosts, 1, com(oldPathIndex, :) );
                    % check for intersections
                    test_nrPathsCrossed = 0;
                    %test_pathIndex = 0;
                    for previousSolutions = 1:length(solutions)
                        if ~isempty(intersect(test, solutions(previousSolutions).path))
                            test_nrPathsCrossed = test_nrPathsCrossed + 1;
                        end
                    end
                    if test_nrPathsCrossed == 0
                       solutions(oldPathIndex).path = test;
                       linkCosts(test) = linkCosts(test) + penalty;
                    end
                else % if oldPathCost is better, discard newPath
                    solutions(contactPair).path = NaN;
                    
                end
            end

            if nrPathsCrossed > 1
                solutions(contactPair).path = NaN;
            end
        end   
    end
    
    nl = [];
    okcom = [];
    nrContactPairs = 0;
    for contactPair = 1:k
        if ~isnan(solutions(contactPair).path)
            nl = [nl; solutions(contactPair).path];
            okcom = [okcom; com(contactPair, :)];
            nrContactPairs = nrContactPairs + 1;
        end
    end
    
    %Adding weight to the 
    ReusedNodes = [];
    for contactPair = 1:k
        if isnan(solutions(contactPair).path)
            newPath = gsp(dimX, dimY, linkCosts, 1, com(contactPair, :) );
            ReusedNodes = [ReusedNodes; intersect(nl, newPath)];
        end
    end
    %visagrid(dimX, dimY, nl, okcom, linkCosts, 25);
    %linkCosts(linkCosts >= 200)
    
    