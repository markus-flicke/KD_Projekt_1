% PREY ALGORITHM
% LFG

clc, clear all, close all
global M MAXPreyTypes NNeighbors

%% ----------------- Generate data and dissimilarity matrix ---------------
% Generate data
disp('Generating data ...')
ScaleBetween = [-1 1];
[X,labels] = gen_data('gaussian2',ScaleBetween,false);



% Dissimilarity Metric
disp('Calculating dissimilarity matrix ...')
metric = 'euclidean'; %'euclidean', 'cosine', 'mahalanobis', 'locality-improved_kernel', 'polynomial_kernel'
DisMatrix = GenDissimilarityMatrix(X,metric);





%% ----------------------- Algorithm parameters ---------------------------
Na   = 3;                                     % Number of agents
M    = ceil(sqrt(10*size(DisMatrix,1)));      % Grid size (MxM)
tmax = 30e3;                                 % Maximum number of iterations
MAXPreyTypes = 100;                           % Number of task types
MaxNeighbors = 15;                             % Maximum number of neighboring objects
%GP = 7;                                      % GP^2 is the size of the main grid partitions
Nclusters = 3;                                % Number of classes in the dataset


%% Repeat algorithm several times
Nrep = 1;
thr_drop=cell(1,Nrep); thr_pick=cell(1,Nrep);
for rep=1:Nrep %Repeat the algorithm several times
    disp(sprintf('Execution number %i',rep))


    %% ------------------------- Initialization -------------------------------
    %%% Define structure with grid information
    InfoGrid = cgrid(DisMatrix);

    %%% Agent definition
    agents(1:Na) = struct('mode','pick','load',[],'fullinformation',[],'HandlingTime',0);

    %%% Updating flag
    uflags = true(1,Na);  % Set 'updating flags' to true

    %%% Agent activaton flag
    aaflags = true(1,Na); % It desactivates agents when maximum number of iterations is reached


    
    %% ---------------------------- Main loop ---------------------------------
    disp('Main algorithm ...')
    %tmax=200e3;
    t=0;  % Current iteration
    k=0;  % Current mapping update
    k2=0; % Current spatial entropy update
    while (any(aaflags))
        t=t+1;

        %%% Number of objects in the neighborhood - dependient on current iteration
        %NNeighbors = round(3+(t-1)*(MaxNeighbors-3)/(tmax-1));
        NNeighbors = MaxNeighbors;

        %%% Agents loop
        for i=1:Na
          
          %%%'Picking' agents
            if (strcmp(agents(i).mode,'pick') && aaflags(i)==true)
                if agents(i).HandlingTime == 0
                    if any(uflags)
                        % Update 'similarity density' in occupied sites
                        [f] = osupdate(InfoGrid);
                        agents(i).fullinformation = f;
                    end;

                    % Generate random index of empty sites vector
                    posOS   = round((length(InfoGrid.OccupiedSites)-1)*rand +1);
                    posGrid = InfoGrid.OccupiedSites(posOS);

                    % Make a decision based on the prey model
                    [decision, HandlingTime,thr] = prey(posOS,agents(i).fullinformation,'pick'); % Prey algorithm
                    thr_pick{rep}=[thr_pick{rep} thr];
                    agents(i).HandlingTime   = HandlingTime;                                     % Assign Handling Time

                    if decision==true
                        % set the updating flag to true
                        uflags(i) = true;

                        % Load agent with object and change mode
                        agents(i).load  = InfoGrid.Grid(posGrid);
                        agents(i).mode  = 'drop';

                        % Empty selected site out from the grid
                        InfoGrid.Grid(posGrid) = 0;

                        InfoGrid.EmptySites    = [InfoGrid.EmptySites posGrid];
                        InfoGrid.OccupiedSites = [InfoGrid.OccupiedSites(1:posOS-1) InfoGrid.OccupiedSites(posOS+1:end)];
                    else
                        % Set the updating flag to false
                        uflags(i) = false;
                    end;
                else
                    agents(i).HandlingTime = agents(i).HandlingTime-1;
                end;

          %%%'Dropping' agents
            elseif strcmp(agents(i).mode,'drop')
                if agents(i).HandlingTime==0
                    if any(uflags)
                        % Update 'similarity density' in empty sites
                        [f] = esupdate(InfoGrid,agents(i).load);
                        agents(i).fullinformation = f;
                    end;

                    % Generate random index of occupied sites vector
                    posES = round((length(InfoGrid.EmptySites)-1)*rand +1);
                    posGrid = InfoGrid.EmptySites(posES);

                    % Make a decision based on the prey model
                    [decision, HandlingTime,thr] = prey(posES,agents(i).fullinformation,'drop');  % Prey algorithm
                    thr_drop{rep}=[thr_drop{rep} thr];
                    agents(i).HandlingTime   = HandlingTime;                                      % Assign Handling Time

                    if decision==true
                        % Set the updating flag to true
                        uflags(i) = true;

                        % Occupy site
                        InfoGrid.Grid(posGrid)= agents(i).load;

                        InfoGrid.EmptySites    = [InfoGrid.EmptySites(1:posES-1) InfoGrid.EmptySites(posES+1:end)];
                        InfoGrid.OccupiedSites = [InfoGrid.OccupiedSites posGrid];

                        % Unload agent
                        agents(i).load  = [];
                        agents(i).mode = 'pick';

                        if t>=tmax
                            aaflags(i) = false;
                        end;
                    else
                        % Set the updating flag to false
                        uflags(i) = false;
                    end;
                else
                    agents(i).HandlingTime = agents(i).HandlingTime-1;
                end;
            end;
        end;

        
        %%% Show and save current information each 5000 iterations
        if(mod(t-1,5000)==0 )
            k=k+1;
            gridt{k}=InfoGrid.Grid;
            disp(sprintf('  Iteration: %i; Activation flags: %s',t,int2str(aaflags)))
        end;

        %%% Calculate current spatial entropy each iteration
%         if(mod(t-1,t-1)==0)
%             k2=k2+1;
%             ent(rep,k2)=spatial_entropy(InfoGrid.Grid,GP);
%         end;
    end; % End for



    %% ------------------------- Save results ---------------------------------
    % Save last grid information
    gridt{end+1}=InfoGrid.Grid;
%    ent(rep,end+1)=spatial_entropy(InfoGrid.Grid,GP);
    plot_grid(gridt{end},labels,M)
    % Convert symbolic grid into a dataset
    X_prey = grid2data(gridt{end},labels,M);
    
%     % Estimate clustering error in mapping
%     LabelsMap = FindLabels(X_prey(:,[1 2]), Nclusters);
%     error(rep) = ILObjects(labels, LabelsMap);
%     save ErrorIC-iris;

    
    % Calculate dissimilarity matrix of mapping and residual variance
%     DisMatrix_prey{rep}=L2_distance(X_prey(:,[1 2])',X_prey(:,[1 2])');
%     rv(rep) = residual_variance(DisMatrix,DisMatrix_prey{rep});
    

    

end; % End while