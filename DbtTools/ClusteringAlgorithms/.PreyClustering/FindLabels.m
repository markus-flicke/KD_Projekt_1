function [LabelsMap] = FindLabels(Xmap, Nclusters)
% FindLabels estimates the labels of each point in the Xmap dataset,
% assuming there are Nclusters


Y = pdist(Xmap,'euclid');
Z = linkage(Y,'complete');
LabelsMap = cluster(Z,'maxclust',Nclusters);