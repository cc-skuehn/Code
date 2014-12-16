function [J, grad] = twiago_cofiCostFunc(params, Y, R, num_ads, num_adspaces, ...
                                  num_features, lambda)

%COFICOSTFUNC Collaborative filtering cost function
%   [J, grad] = COFICOSTFUNC(params, Y, R, num_ads, num_adspaces, ...
%   num_features, lambda) returns the cost and gradient for the
%   collaborative filtering problem.
%   params must contain the concatenation of X(:) and Theta(:)

% Unfold the U and W matrices from params
X = reshape(params(1:num_adspaces*num_features), num_adspaces, num_features);
Theta = reshape(params(num_adspaces*num_features+1:end), ...
                num_ads, num_features);

            
% You need to return the following values
J = 0;
X_grad = zeros(size(X));
Theta_grad = zeros(size(Theta));

% Cost function
C = ((X * Theta')-Y).*R;
J = (1/2)* sum(sum(C.^2));

% Regularized
J = J + (lambda/2)*(sum(sum(Theta.^2))+sum(sum(X.^2)));

% Gradients
% Fully vectorized
X_grad =  C*Theta + lambda.*X;
% CAUTION: Transpose trick for
C = C';
% Fully vectorized
Theta_grad =  C*X + lambda.*Theta;

% =============================================================

grad = [X_grad(:); Theta_grad(:)];

end
