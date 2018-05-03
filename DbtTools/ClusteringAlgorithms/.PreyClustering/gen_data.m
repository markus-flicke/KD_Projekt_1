function [X, labels] = gen_data(option, ScaleBetween, scale)
% GEN_DATA generates data with different shapes
% Option is the data type and scaling_type allows to select type of data scaling

switch option
    case 'gaussian1',
        %% Using drtoolbox
                [X, labels] = generate_data('3d_clusters',100,0);
                X=X(1:65,:); labels = labels(1:65);
        %load datos;
        %X=data;
    case 'gaussian2',
%         %%2D
%         X=[ [normrnd(0.8,0.1,10,1)  normrnd(-0.8,0.1,10,1)];
%            [normrnd(1.5,0.1,10,1)  normrnd(1.5,0.1,10,1)];
%            [normrnd(-1.5,0.1,10,1) normrnd(1.5,0.1,10,1)]];

        %%3D
        n=100;
        X=[[normrnd(-5,0.1,n,1)  normrnd(-5,0.1,n,1) normrnd(-1,0.1,n,1)];
            [normrnd( 5,0.1,n,1)  normrnd( 5,0.1,n,1) normrnd(-1,0.1,n,1)];
            [normrnd(-5,0.1,n,1)  normrnd( 5,0.1,n,1) normrnd( 1,0.1,n,1)]];

        %% Vector containing the label of each cluster
        labels = [1*ones(n,1);
            2*ones(n,1);
            3*ones(n,1)];


    case 'difficult',
        %         %% Using prtools
        %         A=gendatd([50 50],3,10,10);
        %         As=struct(A);
        %         X=As.data; labels = As.nlab;

        %% Using an external funciton
        %[X, labels] = toy2(50);
        %load diff_dm5_var1-5_1-5;
        load diff_dm10_var2-0--4-4_1-4-0-4;
    case 'swiss',
        %% Using drtoolbox
        [X, labels] = generate_data('swiss',1000,0);

    case 'swiss-thin',
        %% Using a data file
        load('Databases\swiss-thin')

    case 'twinpeaks',
        %% Using drtoolbox
        [X, labels] = generate_data('twinpeaks',100,0);

    case 'toy',
        %% Using an external function
        [X, labels] = toy(50);

    case 'iris',
        %% Using a data file
        load('Databases\iris');

    case 'faces',
        %%% Using a data file
        %load('Databases\face_data');
        load('Databases\dsface-sep_data');

        N=50;
        for k=1:N
            %X(k,:)=images(:,k)';
            %X(:,:,k) = reshape(images(:,k),64,64);
            X(k,:) = reshape(images(:,:,k),1,24*24);
        end;
        labels = 1:N;

    case 'olivetti-small',
        load('Databases\olivettismall2');

        X=X(:,:,[1:20 31:40]);%31:40]);
        N=30;
        for k=1:N
            im=X(:,:,k);
            Y(k,:)=im(:);
        end;
        X=Y;

        %        load('Databases\olivettifaces');
        %        X=faces';
        %        X=X(1:40,:)/256;


        labels=[1*ones(10,1);
            2*ones(10,1);
            3*ones(10,1)];
        %                 4*ones(10,1)];
        %                 5*ones(10,1)];
        %                 6*ones(10,1)];
        %                 7*ones(10,1)];
    case 'olivetti',
        load('Databases\olivettifaces');
        pos=[11:20 81:90 101:110 131:140 171:180 211:220 261:270  321:330 331:340 361:370];
        %pos=1:400;
        N=length(pos);
        for k=1:N
            X(k,:)=faces(:,pos(k))'/255;
        end;
        labels = ones(10,1)*(1:N/10); 
        labels = labels(:);

    case 'coil-20',
        load('Databases\coil-20_b');

        %objs = 1:20;
        objs = [12 16 19];
        pos  = 1:72;

        Nobjs = length(objs);
        Npos  = length(pos);

        k=0;        
        for i=1:Nobjs
            for j=1:Npos
                k=k+1;
                X(k,:)=double(data{objs(i)}(pos(j),:))/255;
            end;
        end;

        labels = ones(Npos,1)*(1:Nobjs); 
        labels = labels(:);
    case 'ds-faces',
        %%% Using a data file
        load('Databases\dsface-sep-_data');
        N=50;
        for k=1:N
            im=X(:,:,k);
            Y(k,:)=im(:);
        end;
        X=Y;

        %         N=300;
        %         X = X(:,:,1:N);
        %         labels = 1:N;
        labels=[1*ones(14,1);
            2*ones(16,1);
            3*ones(20,1)];

    case 'mnist',
        load('Databases\mnist_all');

        N=30;

        k=0;
        for i=0:9
            images = double(eval(sprintf('test%i',i)));
            for j=1:N
                k=k+1;
                X(k,:)=images(k,:)/255;
                %X(:,:,k) = reshape(images(k,:),28,28)'/255;
            end;
        end;

        labels = ones(N,1)*(1:10); 
        labels = labels(:);

    case 'wine',
        load('Databases\wine');

    case 'wdbc',
        load('Databases\wdbc');
    otherwise,
        disp('There is something wrong')
end;


if(scale==true)
    X = scaling(X,ScaleBetween); % Scale data
end;