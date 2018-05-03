function [] = plot_grid(Grid,labels,M)

ptsymb = {'bv','b*','b+','bo','bs' 'bd' 'rv','r*','r+','ro','rs','rd','mv','m*','m+','mo','ms' 'md',...
          'gv','g*','g+','go','gs' 'gd' 'cv','c*','c+','co','cs','cd' 'yv','y*','y+','yo','ys' 'yd',...
           'kv','k*','k+','ko','ks' 'kd'};
%ptsymb = {'bv','r*','m+','go','cs'};
%ptsymb = {'kv','k*','k+','ko','ks'};
figure,hold on,box on
for i = 1:M
    for j=1:M
        if Grid(i,j)~=0
            %scatter(j,i,5,labels(Grid(i,j)))
            plot(j,i,ptsymb{labels(Grid(i,j))},'LineWidth',0.5)
            %plot(j,i,'x','LineWidth',1)
        end;
    end;
end;
axis([0 M+1 0 M+1]), xlabel('Dimension 1'), ylabel('Dimension 2')
hold off;

% ptsymb = {'bx','r*','m+','go','cs'};
% figure, hold on
% for i = 1:M
%     for j=1:M
%         if Grid(i,j)~=0
%             if X(Grid(i,j),2)<=0
%                 label=1;
%             elseif X(Grid(i,j),1)<=0
%                 label=2;
%             else
%                 label=3;
%             end;
%             plot(j/M,i/M,ptsymb{label},'LineWidth',2)
%         end;
%     end;
% end;
% grid on, axis([0 1 0 1])
% hold off;