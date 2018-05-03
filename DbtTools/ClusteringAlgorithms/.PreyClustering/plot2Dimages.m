function [] = plot2Dimages(X, images, imsize)

N = size(X,1);
Xs = scaling(X, [0.15 0.85]);

dim1 = Xs(:,1);
dim2 = Xs(:,2);

figure;
for i=N:-1:1
    axes('position',[dim1(i) dim2(i) 0.025 0.025],'Xtick',[],'Ytick',[],'box','on') 
    imshow(reshape(images(i,:), imsize(1),imsize(2)))
end;