nl = [];
okcom = [];
nrContactPairs = 0;
for contactPair = 1:k
    if ~isnan(solution(contactPair).path)
        nl = [nl; solution(contactPair).path];
        okcom = [okcom; com(contactPair, :)];
        nrContactPairs = nrContactPairs + 1;
    end
end

visagrid(dimX, dimY, nl, okcom, u, 25);