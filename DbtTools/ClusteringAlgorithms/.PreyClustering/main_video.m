% PREY-EMBEDDING ALGORITHM
% LFG

clc, clear all, close all
global M MAXPreyTypes NNeighbors

%% ----------------- Generate data and dissimilarity matrix ---------------
% Generate data
ScaleBetween = [-1 1];
[X,labels] = gen_data('gaussian1',ScaleBetween);

%Dissimilarity Metric: metric
metric = 'euclidean'; %'cosine', 'mahalanobis'
DisMatrix = GenDissimilarityMatrix(X,metric);




%% ----------------------- Algorithm parameters ---------------------------
Na   = 3;                         % Number of agents
M    = ceil(sqrt(10*size(X,1)));  % Grid size (MxM)
tmax = 10e3;                      % Maximum number of iterations
MAXPreyTypes = 100;               % Number of task types
MaxNeighbors = 9;                 % Maximum number of neighboring objects




%% ------------------------- Initialization -------------------------------
%%% Define structure with grid information
InfoGrid = cgrid(DisMatrix);

%%% Agent definition
agents(1:Na) = struct('mode','pick','load',[],'fullinformation',[],'HandlingTime',0);

%%% Updating flag
uflags = true(1,Na);  % Set 'updating flags' to true

%%% Agent activaton flag
aaflags = true(1,Na); % It desactivate agents when maximum number of iterations is reached

%aviobj = avifile('peli.avi');
%fig = figure; set(fig,'DoubleBuffer','on');
    
%% ---------------------------- Main loop ---------------------------------
t=0; k=0;
while (any(aaflags))
    t=t+1;
  
    % Number of neighbors in the neighborhood - dependient on current iteration
    %NNeighbors = round(3+(t-1)*(MaxNeighbors-3)/(tmax-1));  
    NNeighbors=MaxNeighbors;
    
    % Show and save current information each 500 iterations
     if(mod(t-1,5)==0 )   
       k=k+1;
       plot_grid(InfoGrid.Grid,labels,M)
       A(k) = getframe;%(gcf);
       %aviobj=addframe(aviobj,A(k)); 
       close all;

%         gridt{k}=InfoGrid.Grid;
         disp(sprintf('Iteration: %i; Activation flags: %s',t,int2str(aaflags)))
%         save resultados;
     end;
%     
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
                [decision, HandlingTime] = prey(posOS,agents(i).fullinformation,'pick'); % Prey algorithm
                agents(i).HandlingTime   = HandlingTime;                                 % Assign Handling Time 

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
            if t>=tmax
                aaflags(i) = false;
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
                [decision, HandlingTime] = prey(posES,agents(i).fullinformation,'drop');  % Prey algorithm
                agents(i).HandlingTime   = HandlingTime;                                  % Assign Handling Time 

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

                else
                    % Set the updating flag to false
                    uflags(i) = false;
                end;
            else
                agents(i).HandlingTime = agents(i).HandlingTime-1;
            end;
        end;
    end;
end;



%% ------------------------- Save results ---------------------------------
% Save last grid information
%gridt{end+1}=InfoGrid.Grid;

% Save to a file
% Y          = grid2data(gridt{end},labels,M);
% Y(:,[1 2]) = escalar(Y(:,[1 2]),ScalingType);
% save resultados;