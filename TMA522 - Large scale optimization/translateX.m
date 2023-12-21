function nl = translateX(x, com, k)
nl = [];

for contactPair = 1:k
    nl = [nl; com(contactPair, 2)];
    pathFinished = 0;
    while ~pathFinished
        nextNode = find( x(:, nl(end), contactPair) == 1);
        nl = [nl; nextNode];
        if nextNode == com(contactPair, 1) % kommentar, ibland når den inte hit
            pathFinished = 1;
        end
    end
end


%visagrid(dimX, dimY, nl, com, zeros(dimX*dimY*2, 1), 25)

% Find rows where the sum is larger than 1
%nyttX = x(:,:,1);
%sumsPerRow = sum(nyttX, 2); % Calculate the sum of each row
%rowsWithSumGreaterThanOne = nyttX(sumsPerRow > 1, :);

%disp('Rows where the sum is larger than 1:');
%disp(rowsWithSumGreaterThanOne);