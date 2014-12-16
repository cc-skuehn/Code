% comparison of recommendation and adserver approach

[stp,si]=sort(true_predictions);
[smd,smdi]=sort(model_data');

% 2 weeks model
% try different approaches depending on the number of adspaces
% here: just 1 adspace used occupied after testing
% alternative: best 4 adspaces like during testing
ad_performance_2w = zeros(8,3);
for i=1:8
    ad_performance_2w(i,1)=(2/14) * sum(Y_raw(:,i))/sum(R(:,i)) + (12/14) * (sum(stp(29:32,i)))/4;
    ad_performance_2w(i,2)=(2/14) * sum(Y_raw(:,i))/sum(R(:,i)) + (12/14) * (sum(model_data'(si(29:32,i),i)))/4;
    ad_performance_2w(i,3)=(8/14) * sum(model_data(:,i))/sum(model_index(:,i)) + (6/14) * (sum(smd(29:32,i)))/4;
end
