function [error] = ILObjects(LabelsIn, LabelsMap)

N = length(LabelsIn);

k=0;
for i=1:N-1
    for j=i+1:N
        k=k+1;
        v(k)=xor(LabelsIn(i)==LabelsIn(j),LabelsMap(i)==LabelsMap(j));
    end;
end;

error=sum(v)/length(v);