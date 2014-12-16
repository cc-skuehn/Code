function adspaceList = loadAdspaceList()
%adspaceLIST reads the fixed adspace list in adspace_ids.txt and returns a
%cell array of the words
%   adspaceList = LOADADSPACELIST() reads the fixed movie list in movie.txt 
%   and returns a cell array of the words in movieList.


%% Read the fixed adspace list
fid = fopen('adspace_ids.txt');

%% Store all adspaces in cell array adspace{}
%%n = 33;  % Total number of adspaces in campaign Redunovin/Oxitamin Testkampgane
% Store all adspaces in cell array adspace{}
%% n = 56;  % Total number of adspaces in twiago livetest recommendation
n = 197;  % Total number of adspaces in twiago new livetest recommendation
%allText = textscan(fid,'%s','delimiter','\n');
%n = length(allText{1}); % Total number of adspaces in twiago livetest recommendation

adspaceList = cell(n, 1);
for i = 1:n
    % Read line
    line = fgets(fid);
    % Word Index (can ignore since it will be = i)
    [idx, adspaceName] = strtok(line, ' ');
    % Actual Word
    adspaceList{i} = strtrim(adspaceName);
end
fclose(fid);

end
