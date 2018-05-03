function [matrix] = GenDissimilarityMatrix(X,metric)

switch metric
    case 'euclidean',   % Euclidean
        Y = pdist(X,'euclidean');

        mini=min(Y); maxi=max(Y);
        Y = (Y-mini)/(maxi-mini);
        matrix = squareform(Y);

    case 'cosine',      % Cosine
        Y = pdist(X,'cosine');

        mini=min(Y); maxi=max(Y);
        Y = (Y-mini)/(maxi-mini);
        matrix = squareform(Y);

    case 'mahalanobis', % Mahalanobis
        Y = pdist(X,'mahalanobis');

        mini=min(Y); maxi=max(Y);
        Y = (Y-mini)/(maxi-mini);
        matrix = squareform(Y);

    case 'geodesic', % Geodesic distance
        disp('Geodesic distance')
        D = squareform(pdist(X,'euclidean')); % Calculate euclidean distance
        
        Y = geodesica(D, 'k', 7);             % Calculate geodesic
        mini=min(Y(:)); maxi=max(Y(:));       % Standarize matrix
        matrix = (Y-mini)/(maxi-mini);

    case 'polynomial_kernel', % Polynomial kernel
        d=1.5;
        PK = EuclideanPK(X,d);
        %save 'kernel_pk-15.mat' 'PK';
        %load kernel_faces_pk.mat;

        mini=min(PK); maxi=max(PK);
        Y = (PK-mini)/(maxi-mini);
        
        %Y = -(LIK-maxi)/(maxi-mini);
        matrix = squareform(Y);
        
    case 'locality-improved_kernel', % Locality-improved kernel

        d1=2; d2=2; p=15;
        LIK = EuclideanLIK(X,d1,d2,p);
        %save 'kernels_dsfaces300_lik2-1-15.mat' 'LIK'
        %load 'kernels_dwfaces_lik2-2-13.mat';

        mini=min(LIK); maxi=max(LIK);
        Y = (LIK-mini)/(maxi-mini);

%         %Y = -(LIK-maxi)/(maxi-mini);
%         m=mean(Y); s=std(Y);
%         mini= max(m-2*s,0); maxi=min(1,m+2*s);
%         Y = (Y-mini)/(maxi-mini);
%         Y(Y<0)=0;  Y(Y>1)=1;      
          matrix = squareform(Y); 
    case 'radial-basis_kernel',
        q=5;
        RBK=EuclideanRBK(X,q);

        mini=min(RBK); maxi=max(RBK);
        Y = (RBK-mini)/(maxi-mini);

        matrix = squareform(Y);
    otherwise,
        Y = pdist(X,'euclidean');

        mini=min(Y); maxi=max(Y);
        Y = (Y-mini)/(maxi-mini);

        matrix = squareform(Y);
end;

