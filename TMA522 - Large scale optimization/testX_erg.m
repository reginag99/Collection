function [x_erg, feasible, nrContactPairs] = testX_erg(x_erg, dimX, dimY, k, com)
%returns a rounded x_erg
for l = 1:k
    for i = 1:dimX*dimY*2
        for j = 1:dimX*dimY*2
            if x_erg(i,j,l) > 0.5
                x_erg(i,j,l) = 1;
            else
                x_erg(i,j,l) = 0;
            end
        end
    end
end

[nrContactPairs, feasible] = calculateFx(x_erg, dimX, dimY, k, com);
