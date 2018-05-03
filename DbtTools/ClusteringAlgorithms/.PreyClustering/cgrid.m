function [InfoGrid] = cgrid(DisMatrix)
% CGRID defines a structure with fields 
global M

Nd     = size(DisMatrix,1);    % Number of data points
rndidx = randperm(M*M);        % Vector de posiciones aleatorias

%%% Definir malla toroidal y asignar objetos en ella
Grid   = zeros(M);
for i=1:Nd
    Grid(rndidx(i))=i;
end;

%%% Crear estructura principal que contiene toda la información de la malla
InfoGrid = struct('Grid',Grid,'DisMatrix',DisMatrix,...
                  'OccupiedSites',sort(rndidx(1:Nd)),'EmptySites',sort(rndidx(Nd+1:end)));
