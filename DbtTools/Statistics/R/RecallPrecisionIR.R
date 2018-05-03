RecallPrecisionIR <- function(isRelevant,nrOfAllRelevantItems,nrOfAllNonRelevantItems=0){
warning('New Version is informationRetrieval with IR.PrecisionRecall() in RecallPrecisionIR and IR.AUCandFbetha, see also IR4Terms')
 nrRetrievedItems<-length(isRelevant)
 nrRelevantItems<-sum(isRelevant)
 
 precision<-nrOfRelevantItems/nrRetrievedItems*100
 recall<-nrOfRelevantItems/nrOfAllRelevantItems*100
 
 if((precision+recall)>0){
    fmeasure<-2*(precision*recall)/(precision+recall)
 }else{
    fmeasure<-0
 }
 
 if(nrOfAllNonRelevantItems==0){
    fallout<-Null
 }else{
    nrOfNonRelevantItems<-nrRetrievedItems-nrOfRelevantItems
    fallout<-nrOfNonRelevantItems/nrOfAllNonRelevantItems*100
 }
 
 return (list(precision=precision,recall=recall,fmeasure=fmeasure,fallout=fallout)) 

 }

