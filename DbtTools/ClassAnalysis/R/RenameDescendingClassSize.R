RenameDescendingClassSize <- function(cls) {
#   % Cls are renamed such that largest class =1 ...
#   % RenamedCls = RenameForCoincidence(GivenCls,StandardCls);
#   % INPUT
#   % Cls   vector of classifications
#   % OUTPUT
#   % RenamedCls               such that largest class =1 ...  
#   
#   % AUTHOR ALU 2009
#   % in \dbt\ClassAnalysis
# Author: ?
# 1.Editor: MT
# 2.Editor: Walteran (Anne)
# 3.Editor: MT
  ListeV <- ClassCount(cls)
  countPerClass <- ListeV[[2]]
  UniqueClasses=ListeV[[1]]
  sortedClasses <- sort(na.last=TRUE,countPerClass, decreasing = TRUE, index.return=TRUE) # Original-Indizes mitliefern lassen
  numberOfClasses <- length(countPerClass)
  renamedCls <- cls
  
  for (i in 1: numberOfClasses) {
    # renamedCls[which(cls == sortedClasses[i])] <- i
    
    #Korrektur Anne:
    renamedCls[which(cls == UniqueClasses[sortedClasses$ix[i]],arr.ind = T)] <- i # Hier mit den mitgelieferten Original-Indizes arbeiten
  }  
  return(renamedCls)
}
