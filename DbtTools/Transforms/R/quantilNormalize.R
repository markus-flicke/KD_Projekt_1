`quantilNormalize` <- function(data, standardized=1, referenceDistribution= as.matrix(meanrobust(t(data),0.2))
){

# function NormalizedData = QuantilNormalize(Data,Standardized,ReferenceDistribution);
# % NormalizedData = QuantilNormalize(Data);
# % NormalizedData = QuantilNormalize(Data,ReferenceDistribution);
# % Quantilnormalisierung:  Daten werden gemaess ihrem Rang durch die Werte der ReferenceDistribution ersetzt
# %
# % INPUT
# % Data(1:Cases,1:Variables)      Daten, faelle in den Zeilen, Veriablen in den Spalten
# %
# % OPTIONAL
# % Standardized                   die Referenzverteilung wird z-transformiert(default ==1)
# % ReferenceDistribution(1:Cases) Referenzverteilung
# %
# % OUTPUT
# % NormalizedData(1:Cases,1:Variables)   quantil-normalisierte Daten
# 
# % ALU Dez. 2008
# if nargin < 2 ; Standardized =1; end;
# if nargin < 3 ; % keine Referenzvertilung verwende Mittelwert der Faelle
#     ReferenceDistribution = meanrobust(Data', 0.2)';
# end; 
# 


	if(standardized==1){
		referenceDistribution <- as.matrix(referenceDistribution)
		referenceDistribution <- ztransrobust(referenceDistribution,0.1)
	}


	
# 
# % sortieren der ReferenceDistribution
# ReferenceDistribution = sort(na.last=T,colvector(ReferenceDistribution));  % ab jetzt stimmt dies mit ihrem Rang 
	referenceDistribution <- as.matrix(referenceDistribution)
	dim(referenceDistribution) <- c(dim(referenceDistribution)[1]*dim(referenceDistribution)[2],1)
	referenceDistribution <- sort(na.last=T,referenceDistribution,na.last=TRUE)
		
# % NaN werden ans obere Ende Sortiert

	referenceDistribution[is.nan(referenceDistribution)] = max(referenceDistribution,na.rm=TRUE)
	# ReferenceDistribution = nantovalue(ReferenceDistribution,nanmax(ReferenceDistribution));

	sortedData <- apply(data,2,sort,na.last=T)
	ranks <- apply(data,2,order,na.last=T)


# 
# % Normalisieren


	nCases <- dim(data)[1]
	nVar <- dim(data)[2]
	
	normalizedData <- data;
	
	
# NormalizedData =Data;

	for (i in nVar){
		normalizedData[ranks[,i],i] <- referenceDistribution
	}
# for i=1:AnzVariables
#     NormalizedData(Raenge(:,i),i) = ReferenceDistribution;
# end;
# 
# % NaN auf NaN abbilden

	nanInd <- is.nan(data)
	if (sum(nanInd)>0){
			normalizedData[nanInd] <- data[nanInd]
		}
# NaNind = find(isnan(Data));
# if length(NaNind) >0; NormalizedData(NaNind) =Data(NaNind); end; % NaN -> NaN
# 
# 
# % test Plot
# % [AnzCases,AnzVariables] = size(Data);
# % Min = nanmin(Data(:)); Max = nanmax(Data(:));
# % L = nanmin(NormalizedData(:)); H = nanmax(NormalizedData(:));
# % for i=1:AnzVariables
# %     tileplot(3,4,i);
# %     plot(Data(:,i), NormalizedData(:,i),'.');
# %     grid on; axis([Min,Max,L,H]);
# %     title(num2str(i)); xlabel('Data');ylabel('NormalizedData');
# % end;
# 

return(normalizedData)

 }

