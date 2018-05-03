function[sdata] = scaling(data, ScaleBetween)
% SCALING(data, ScaleBetween) scales features between a range given by ScaleBetween
miny = ScaleBetween(1);
maxy = ScaleBetween(2);
 
j=0;
for i=1:size(data, 2)
    minx=min(data(:,i));
    maxx=max(data(:,i));
    if(minx ~= maxx) %Don't take into account features with same value 
       j=j+1;
       sdata(:,i) = miny + (data(:,i)-minx)*(maxy-miny)/(maxx-minx);
    end;
end;