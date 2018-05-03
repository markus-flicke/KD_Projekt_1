function [ent] = spatial_entropy(Grid,N)
% SPATIAL_ENTROPY calculates the spatial entropy by dividing the MxM Grid
% into sub-grids of size NxN.

M   = size(Grid,1);  % M^2: Grid size

k=0;
for i=1:M/N
    for j=1:M/N
        k=k+1;
        SG   = Grid( (i-1)*N+1:i*N, (j-1)*N+1:j*N ); % Split Grid
        No   = length(find(SG~=0));                  % Calculate number of objects on the sub-grid
        p(k) = No/N^2;                               % Proportion of objects on sub-grid of size NxN
    end;
end;

if(any(p<0) || any(p>1))
    disp('Something is wrong!!!');
end;
ent = -sum(p.*(log(p+eps)/log(N^2)));