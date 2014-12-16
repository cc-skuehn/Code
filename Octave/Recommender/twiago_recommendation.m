%% Machine Learning - Collaborative Filtering
%
%  Instructions
%  ------------

%% =============== Part 1: Loading adspace CTR dataset ================
%  

format long

fprintf('Loading adspace CTR dataset.\n\n');

%  Load data
Y_raw = load ("recom.matrix")';
R = logical(load ("index.matrix")');


Y = (Y_raw-min(Y_raw(:)))/(max(Y_raw(:))-min(Y_raw(:)));
Y(R<1) = 0;

%  From the matrix, we can compute statistics like average rating.
fprintf('Average scaled CTR for Adspace 1: %f \n\n', ...
        mean(Y(1, R(1, :))));
if 0
 %  We can "visualize" the CTRs matrix by plotting it with imagesc
 imagesc(Y);
 ylabel('Adspaces');
 xlabel('Ads');

end % if 0

if 0
adspaceList = loadAdspaceList();
end

%% ================== Part 2: Learning CTRs ====================
%  Now, we will train the collaborative filtering model on our example datsa set of 33 adspaces and 8 ads
%  For more info on the data (Y,R) see above

%fprintf('\nTraining collaborative filtering...\n');

%  Useful Values
num_ads = size(Y, 2);
num_adspaces = size(Y, 1);
num_features = 150;

%  Normalize CTRs - Mean Normalization
[Ynorm, Ymean] = normalizeCTRs(Y, R);
%Ymean = zeros(1,num_ads);

% Test Normalization
Ytest = zeros(size(Ynorm));
for i=1:num_ads
    Ytest(:,i) = Ynorm(:,i) + Ymean;
end
Ytest(R<1) = 0; 
y_norm_err = sum(abs(Ytest(:)-Y(:)));

% Set Initial Parameters (Theta, X)
X = randn(num_adspaces, num_features);
Theta = randn(num_ads, num_features);
%X = X_sav;
%Theta = Theta_sav;
X_sav = X;
Theta_sav = Theta;

initial_parameters = [X(:); Theta(:)];

% Set options for fmincg
options = optimset('GradObj', 'on', 'MaxIter', 300);

%%% LOOP in order to find best lambda 
%%% suppress any printings 
% Set Regularization

len_loop = 10;
avg_err_coll = zeros(len_loop+1,3); 

#for k=1:len_loop+1
for k=4:5
 lambda = 0.0005+(k-1)*0.0005;

 % Bestes lambda
 # lambda = 0.02;

 theta = fmincg (@(t)(twiago_cofiCostFunc(t, Ynorm, R, num_ads, num_adspaces, ...
                                num_features, lambda)), ...
                initial_parameters, options);


% Unfold the returned theta back into U and W
X = reshape(theta(1:num_adspaces*num_features), num_adspaces, num_features);
Theta = reshape(theta(num_adspaces*num_features+1:end), ...
                num_ads, num_features);

fprintf('Recommender system learning completed: %i\n',k);

%fprintf('\nProgram paused. Press enter to continue.\n');
%pause;

%% ================== Part 3: Recommendations ====================
%  After training the model, we can now make recommendations by computing
%  the predictions matrix.
%

p = X * Theta';
my_predictions = p(:,1) + Ymean;
%my_predictions = p(:,1);

% Denormalize predictions for all combinations
prediction_test = zeros(size(p));
for i=1:num_ads
    prediction_test(:,i) = p(:,i) + Ymean;
end

% Predictions of the scaled input data, use this as evaluation measure
input_test = prediction_test;
input_test(R<1) = 0;

if 0
[r, ix] = sort(my_predictions, 'descend');
fprintf('\nAll recommendations:\n');
for i=1:num_adspaces
    j = ix(i);
    fprintf('Predicting scaled CTR %f for adspace %s\n', my_predictions(j), ...
            adspaceList{j});
end

fprintf('\n\nOriginal scaled CTRs provided:\n');
for i = 1:length(Y_raw(:,1))
    if Y_raw(i,1) > 0 
%        fprintf('Got %f for %s\n', Y_raw(i,1), ...
%                 adspaceList{i});
        fprintf('Got %f for %s\n', Y(i,1), ...
                 adspaceList{i});
    end
end
end % if 0, just for suppressing output

%error_vector = abs(prediction_test(:)(R(:)>0) - Ynorm(:)(R(:)>0));
error_vector = abs( (input_test - Y)(R>0 & Y_raw>0)(:) );
err_vec_rel = error_vector ./ abs(Y(R>0 & Y_raw>0)(:));
error_compare = [prediction_test(R>0 & Y_raw>0)(:) Y(R>0 & Y_raw)>0(:) error_vector err_vec_rel];
total_rel_error = sum(err_vec_rel);
%average_rel_error = total_rel_error/sum(R(:));
average_rel_error = total_rel_error/sum(Y_raw(:)>0);
res_lambda = [lambda total_rel_error average_rel_error];
avg_err_coll(k,:) = res_lambda;

% Unscale the predictions
true_predictions = prediction_test * (max(Y_raw(:))-min(Y_raw(:))) + min(Y_raw(:));
true_input_predictions = true_predictions;
true_input_predictions(R<1) = 0;

#k
#true_predictions(81:83,112:114)
#Y_raw(81:83,112:114)

%[k sum(abs((Y_raw-true_predictions)(R>0))) sum(abs((Y_raw-true_predictions)(R>0)))/sum(R(:)>0)]
end % end lambda for loop

%Y_raw(1,R(1,:)>0)
%true_predictions(1,R(1,:)>0)
%Y_raw(1,R(1,:)>0)-true_predictions(1,R(1,:)>0)
%true_predictions(14:16,1:20)
%Y_raw(14:16,1:20)
if 1
test_preds = true_predictions;
save("test_pred.mat","test_preds")
#save("test_pred.mat","true_predictions")
end
#true_predictions(14:16,1:7)
#Y_raw(14:16,1:7)
#avg_err_coll(k,:)