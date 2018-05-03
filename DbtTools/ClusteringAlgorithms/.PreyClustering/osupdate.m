function [f_os] = osupdate(InfoGrid)
global NNeighbors


f_os = osupdateC(InfoGrid.Grid, InfoGrid.OccupiedSites, InfoGrid.DisMatrix, NNeighbors);

% N    = length(InfoGrid.OccupiedSites);
% f_os = zeros(1,N); 
% 
% for i=1:N
%     [posr,posc] = single2coord(InfoGrid.OccupiedSites(i),M,M);
%     oi          = InfoGrid.Grid(posr,posc);                                       % Object on the position (posr,posc)
%     [ojsAttrIndex,ojsDCB] = neighborsC2(InfoGrid.Grid,posr,posc,NNeighbors);      % Neighborhood 'ojs' surrounding object 'oi'
%     f_os(i)               = densityC(oi,ojsAttrIndex,ojsDCB,InfoGrid.Attributes); % Calculate similarity between object and its neighborhood
% end;