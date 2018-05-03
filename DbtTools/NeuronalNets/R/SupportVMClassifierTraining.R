SupportVMTraining <- function(Data,Cls){
requireNamespace('e1071')
  erg <-   e1071::svm(x=Data,y=factor(Cls))
return(erg)

# function  [NNCls,Accuracy,MLPBP] = BackPropClassifierTraining(Data,Cls,HiddenNeurons,Epochs,PreTrainedMLBP);
# % [NNCls,Accuracy,MLPBP] = BackPropClassifierTraining(Data,Cls,HiddenNeurons,Epochs,PreTrainedMLBP);
# %  ein Klassifikator als MLP mit Back-Propagation
# % INPUT
# % Data(1:n,1:d)          cases in rows, variables in columns
# % Cls(1:n)               corresponding classification
# % HiddenNeurons          Vector of number of neurons in hidden layer e.g. [ 3 3]
# % Epochs                 number of training epochs
# % OPTIONAL               
# % PreTrainedMLBP         MLPBP which has been trained bevore
# % OUTPUT
# % NNCls                  Klassifikation des MLPBP nach dem Training
# % Accuracy               Performanz als % richtige
# % MLPBP                  das MLP mit Back-Propagation wie in den Mathlabfunktionen
# 
# % Voreinstellungen
# ActivationFunction           = 'tansig' ; % sigmoid im Bereich +- 1
# BPTrainingFunction           = 'trainrp';  % traingdx     Gradient descent with momentum and adaptive lr backpropagation.  (DEFAULT)
# BPWeightBiasLearningFunction = 'learngdm'; % Gradient descent w/momentum weight/bias learning function. (andere siehe unten)
# PerformanceFunction          = 'mse';      % mse          Mean squared error-performance function. (andere siehe unten)
# 
# 
# [AnzDaten AnzVariablen] = size(Data);
# [UniqueClasses,CountPerClass,NrOfClasses] = ClassCount(Cls);
# 
#  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  % Spezifizieren des Netzes
#  MinMaxRanges = minmax(Data');  % Min & Max as Ranges spezifizieren
#  NrOfInputNeurons  =  AnzVariablen;
#  NrOfOutputNeurons =  NrOfClasses;
#  NeuronLayerSize = [ NrOfInputNeurons ,  HiddenNeurons,  NrOfOutputNeurons ];
#  NumberOfLayers = length(NeuronLayerSize);
#  ActivationFunction = ones(NumberOfLayers,1)*ActivationFunction ; 
#  ActivationFunctionLayers =cellstr(char(ActivationFunction))' ;
# 
# 
# if  nargin>4 % PreTrainedMLBP given
#     MLPBP=PreTrainedMLBP;
# else  %  Multilayer Perzepron Backprop Feed Forward Netz neu generieren
#     MLPBP=newff( MinMaxRanges , NeuronLayerSize,  ActivationFunctionLayers, ...
#                 BPTrainingFunction, BPWeightBiasLearningFunction,PerformanceFunction);
# end;
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % Input & Output Pattern 
#  InputPattern = Data';
#  % 1 aus n umcodierung
#  TargetPattern = -ones(NrOfClasses,AnzDaten); % all set to -1
#  for i=1:AnzDaten
#      ColumnNr = find(Cls(i)==UniqueClasses);
#      TargetPattern(ColumnNr,i) =1;            % class set to +1
#  end;     
#  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  % Training des Netzes
#  MLPBP.trainParam.epochs = Epochs ;% Trainingsepochen
#  MLPBP = train(MLPBP,InputPattern,TargetPattern);
# 
#  
#  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# % Ausgabe erzeugen 
#  NetOutput = sim(MLPBP,InputPattern);
#  
# % Output in KLassen zurueckfuehren
# [Max MaxClassInd] = max( NetOutput);
# NNCls = Cls*0;
# for i=1:AnzDaten
#   NNCls(i) = UniqueClasses(MaxClassInd(i));
# end;     
# 
# Accuracy = sum(NNCls == Cls)/ AnzDaten *100;
# 
# 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %DOKUMENTATION DER Mathlab NN Funktionen
#  ActivationFunctions         = {'logsig','logsig','logsig','logsig'};
# % Activation Functions (see Neural Network Toolbox / Transfer Functions)
# % compet           Competitive transfer function.
# % hardlim          Hard limit transfer function.
# % hardlims         Symmetric hard limit transfer function
# % logsig           Log sigmoid transfer function.
# % poslin           Positive linear transfer function
# % purelin          Linear transfer function.
# % radbas           Radial basis transfer function.
# % satlin           Saturating linear transfer function.
# % satlins          Symmetric saturating linear transfer function
# % softmax          Softmax transfer function.
# % tansig           Hyperbolic tangent sigmoid transfer function.
# % tribas           Triangular basis transfer function.
# 
#  BPTrainingFunction           = 'traingdx'; 
# % Training Functions
# % trainbfgBFGS quasi-Newton backpropagation.
# % trainbr      Bayesian regularization.
# % traincgb     Powell-Beale conjugate gradient backpropagation.
# % traincgf     Fletcher-Powell conjugate gradient backpropagation.
# % traincgp     Polak-Ribiere conjugate gradient backpropagation.
# % traingd      Gradient descent backpropagation.
# % traingda     Gradient descent with adaptive lr backpropagation.
# % traingdm     Gradient descent with momentum backpropagation.
# % traingdx     Gradient descent with momentum and adaptive lr backpropagation.  (DEFAULT)
# % trainlm      Levenberg-Marquardt backpropagation.
# % trainoss     One-step secant backpropagation.
# % trainrp      Resilient backpropagation (Rprop).
# % trainscg     Scaled conjugate gradient backpropagation.
# % trainb       Batch training with weight and bias learning rules.
# % trainc       Cyclical order incremental training with learning functions
# % trainr       Random order incremental training with learning functions.
# 
# BPWeightBiasLearningFunction = 'learngdm';
# % Learning Functions
# % learncon         Conscience bias learning function.
# % learngd          Gradient descent weight/bias learning function.
# % learngdm         Gradient descent w/momentum weight/bias learning function.
# % learnh           Hebb weight learning function.
# % learnhd          Hebb with decay weight learning rule.
# % learnis          Instar weight learning function.
# % learnk           Kohonen weight learning function.
# % learnlv1         LVQ1 weight learning function.
# % learnlv2         LVQ2 weight learning function.
# % learnos          Outstar weight learning function.
# % learnp           Perceptron weight and bias learning function.
# % learnpn          Normalized perceptron weight and bias learning function.
# % learnsom         Self-organizing map weight learning function.
# % learnwh          Widrow-Hoff weight and bias learning rule.
# % 
# PerformanceFunction          = 'mse';
# % Performance Functions
# % mae          Mean absolute error-performance function.
# % mse          Mean squared error-performance function.
# % msereg       Mean squared error w/reg performance function.
# % sse          Sum  squared error-performance function.
# % 

# Data, Cls, HiddenNeurons, Epochs, PreTrainedMLBP






 

 }

