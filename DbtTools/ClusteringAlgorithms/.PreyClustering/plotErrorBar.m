function [] = plotErrorBar(x)

%N = 12611;
N = 4275;
M = length(x);
for i=1:M
       y(i,:)=x{i}(1:N); 
end;
size(y,2)
y=y(:,1:126:end);
size(y,2)
X=round(linspace(1,20e3,size(y,2)));Y=mean(y);D=std(y)*0.5;errorbar(X,Y,D)
