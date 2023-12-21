function [diff, sumOut] = checkNeighbours(i, l, x, dimX, dimY)
    sumOut = 0;
    sumIn = 0; % this is the sum of the neighbours
    if i <= dimX*dimY
        if mod(i,dimX) ~= 0
            sumOut = sumOut + x(i+1, i, l);
            sumIn = sumIn + x(i, i+1, l);
        end
        if mod(i,dimX) ~= 1
            sumOut = sumOut + x(i-1, i, l);
            sumIn = sumIn + x(i, i-1, l);
        end
        
        modulogrej = mod(i,dimX);
        if modulogrej == 0
            modulogrej = dimX;
        end
        j = dimX*dimY+dimY*(modulogrej-1) + ceil(i/dimX); 
        sumOut = sumOut + x(j, i, l);
        sumIn = sumIn + x(i, j, l);
    else
        if mod(i,dimY) ~= 0
            sumOut = sumOut + x(i+1, i, l); 
            sumIn = sumIn + x(i, i+1, l);
        end
        if mod(i,dimY) ~= 1
            sumOut = sumOut + x(i-1, i, l);
            sumIn = sumIn + x(i, i-1, l);
        end
        
        modulogrej = mod(i-dimX*dimY, dimY);
        if modulogrej == 0
            modulogrej = dimY;
        end
        j = (modulogrej-1)*dimX+ceil((i-dimX*dimY)/dimY); 
        sumOut = sumOut + x(j, i, l);
        sumIn = sumIn + x(i, j, l);
    end
    diff = sumOut - sumIn;
end