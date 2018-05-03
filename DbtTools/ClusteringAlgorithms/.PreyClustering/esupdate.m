function [f_es] = esupdate(InfoGrid,oi)
% ESUPDATE calculates similarity density in empty sites, with respect to oi
global NNeighbors


f_es = esupdateC(InfoGrid.Grid, InfoGrid.EmptySites, InfoGrid.DisMatrix, oi, NNeighbors);


% N=length(InfoGrid.EmptySites);
% for i=1:N
%     [posr,posc] = single2coord(InfoGrid.EmptySites(i),M,M);
%     [ojsAttrIndex,ojsDCB] = neighborsC2(InfoGrid.Grid,posr,posc,NNeighbors);      % Neighborhood 'ojs' surrounding object 'oi'
%     f_es(i)               = densityC(oi,ojsAttrIndex,ojsDCB,InfoGrid.Attributes); % Calculate similarity between object and its neighborhood
% end;