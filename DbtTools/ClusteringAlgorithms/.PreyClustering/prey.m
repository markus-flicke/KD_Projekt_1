function [decision, HandlingTime,threshold] =prey(MetPreyIndex,DensityInfo,mode)
global MAXPreyTypes


%%% Calculate model parameters
environment_ptypes = density2type(DensityInfo,mode);

lamda  = type_rates(environment_ptypes)'; % Encounter rates
i      = (1:MAXPreyTypes)';               % Prey types
e      = type2time(i);                    % Handling times
v      = type2gain(i);                    % Gain points
profit = v./e;                            % Profitability 

%%% Sort parameters according to profitability
% total = sortrows([profit i e v lamda],1);
% 
% profit = total(end:-1:1,1);
% i      = total(end:-1:1,2);
% e      = total(end:-1:1,3);
% v      = total(end:-1:1,4);
% lamda  = total(end:-1:1,5);

%%% Calculate lowest ranking prey type in the diet
opplost = cumsum(lamda.*v)./(1+cumsum(lamda.*e)); % Lost of opportunity


j = find((opplost(1:end-1)<=profit(2:end))==0);  
if(isempty(j))
    j=MAXPreyTypes;
else
    j=j(1);
end;

%%% Calculate attack probability
if j==MAXPreyTypes || j==1
    p=true(1,MAXPreyTypes);
else    
    p = [true(1,j) false(1,MAXPreyTypes-j)]; 
end;


%%% Decide if attack or not
MetPrey      = environment_ptypes(MetPreyIndex);
decision     = p(i==MetPrey);

%threshold=opplost(j)*decision;
threshold=profit(j);


if decision == false
  HandlingTime = 0;
else
  HandlingTime = 0*e(i==MetPrey);
end;



%--------------------------------------------------------------------------
function [ptype] = density2type(f, mode)
global MAXPreyTypes

if strcmp(mode,'pick')
    ptype = round((MAXPreyTypes-1)*f +1);
else
    ptype = round((1-MAXPreyTypes)*f + MAXPreyTypes);
end;


%--------------------------------------------------------------------------
function [v] = type2gain(ptype)
global MAXPreyTypes

v=MAXPreyTypes+1-ptype;


%--------------------------------------------------------------------------
function [e] = type2time(ptype)
%global MAXPreyTypes

% a = log(1/5)/(1-MAXPreyTypes);
% e = round( 5*exp(-a*(ptype-1)) );

%e = round((((MAXPreyTypes-ptype)/(MAXPreyTypes-1)).^5)*4+1);
e=ones(size(ptype,1),size(ptype,2));

%--------------------------------------------------------------------------
function [lamda] = type_rates(ptype)
global MAXPreyTypes

lamda = histc(ptype,1:MAXPreyTypes);

% lamda = zeros(1,MAXPreyTypes);
% for i = 1:MAXPreyTypes
%     lamda(i)=length(find(ptype==i));
% end;