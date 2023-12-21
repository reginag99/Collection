function [newcom, newnl] = GetFeasiblesolution2(dimX, dimY, u, k, com, nl)
    nrNodes = dimX*dimY*2;
    
    oldu = u;
    newnl = nl;
    newcom = com;
    
    %counts number of times a node has been used in all of the paths
    timesUsed = zeros(size(u));
    for i=1:nrNodes
        timesUsed(i) = length(find(nl == i));
    end
    
    % if no node has been passed more than once we are good
    if (max(timesUsed) < 2)
        return
    end
    
    % if not find nodes that have been passed more than once
    crossingPoints = find(timesUsed > 1);
    
    % as long as we have nodes that have been passed more than once
    while(~isempty(crossingPoints))
        
        % iterate through all nodes that are crossingPoints 
        for nodeIndex=1:length(crossingPoints)
            nl = newnl;
            com = newcom;
            u = oldu;       
            
            
            % find a path that cross another 
            node = crossingPoints(nodeIndex);
            first = 0; % start index in nl
            last = 0; % end index in nl 
            startNode = 0; % start of path
            endNode = 0; % end of path
            for i = 1:size(com,1)
                first = last+1;
                slask = find(nl(last+1:length(nl)) == com(i,1));
                last = slask(1)+first-1;
                if sum(nl(first:last) == node) > 0
                    endNode = nl(first);
                    startNode = nl(last);
                    break
                end
            end
            
            % remove path from nl
            nl(first:last) = [];
            com(ismember(com, [startNode endNode], 'rows') == 1, :) = [];
            
            % sets cost of the other paths used nodes to inf so we avoid
            % them in our new solution
            u(nl) = inf;
            
            % find new path !
            newnl = gsp(dimX, dimY, u', 1, [endNode startNode]);
            
            % if a path wasnt found, keep removing more paths 
            if length(newnl) < 2 || sum(u(newnl)) == inf % 
                continue                                  
            else                                          
                break                                     
            end
        end
        
        

        % add new path to nl if we found one
        if length(newnl) >= 2 && sum(u(newnl)) ~= inf % 
            % Append the found path to com/nl
            newcom = [com; startNode endNode];
            newnl = [nl; flipud(newnl)];
        else
            newcom = com;
            newnl = nl;
        end
        
        % update number of intersections for our outer loop
        for i=1:length(timesUsed)
            timesUsed(i) = length(find(nl == i));
        end
        crossingPoints = find(timesUsed > 1);
    end