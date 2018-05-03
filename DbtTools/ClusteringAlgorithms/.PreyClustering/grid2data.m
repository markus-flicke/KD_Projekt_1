function [Y] = grid2data(Grid,labels,M)

k=0;
for i = 1:M
    for j=1:M
        if Grid(i,j)~=0
            k=k+1;
            Y(k,:)=[j/M i/M labels(Grid(i,j))];
        end;
    end;
end;