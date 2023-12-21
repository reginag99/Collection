function gamma = CalculateSubGradientDirection(x, k, dimX, dimY)
n = 2*dimX*dimY;
gamma = ones(1, n);

for i = 1:n
    sum = 0;
    for l = 1:k
        if i <= dimX*dimY
            if mod(i,dimX) ~= 0
                sum = sum + x(i+1,i,l);
            end
            if mod(i,dimX) ~= 1
                sum = sum + x(i-1,i,l);
            end

            modulogrej = mod(i,dimX);
            if modulogrej == 0
                modulogrej = dimX;
            end
            j = dimX*dimY+dimY*(modulogrej-1) + ceil(i/dimX);
            sum = sum + x(j, i, l);
        else
            if mod(i,dimY) ~= 0
                sum = sum + x(i+1, i, l);
            end
            if mod(i,dimY) ~= 1
                sum = sum + x(i-1,i,l);
            end

            modulogrej = mod(i-dimX*dimY, dimY);
            if modulogrej == 0
                modulogrej = dimY;
            end
            j = (modulogrej-1)*dimX+ceil((i-dimX*dimY)/dimY);
            sum = sum + x(j, i, l);
        end
    end
    gamma(i) = gamma(i) - sum;
end
