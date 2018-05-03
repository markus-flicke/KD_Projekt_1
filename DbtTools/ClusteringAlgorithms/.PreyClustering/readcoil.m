clc, clear all;
for i=1:20
    for j=0:71
        im = imread(sprintf('obj%i__%i',i,j),'png');
        datat(j+1,:)=im(:)';
    end;
    data{i}=datat;
    clear datat;
end;




% clc, clear all;
% k=0;
% for i=1:20
%     for j=0:71
%         k=k+1;
%         im = imread(sprintf('obj%i__%i',i,j),'png');
%         data(k,:)=im(:)';
%     end;
% end;
% 
