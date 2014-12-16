function [Ynorm, Ymean] = normalizeCTRs(Y, R)
%NORMALIZECTRS Preprocess data by subtracting mean rating for every 
%AdSpace (every row)
%   [Ynorm, Ymean] = NORMALIZECTRS(Y, R) normalized Y so that each Adspace
%   has a CTR of 0 on average, and returns the mean CTR in Ymean.
%

[m, n] = size(Y);
Ymean = zeros(m, 1);
Ynorm = zeros(size(Y));
for i = 1:m
    test_vec = sum(R(i, :) == 1);
    if test_vec==0
       Ymean(i) = 0;
    else 
    idx = find(R(i, :) == 1);
    Ymean(i) = mean(Y(i, idx));
    Ynorm(i, idx) = Y(i, idx) - Ymean(i);
    end
end

end
