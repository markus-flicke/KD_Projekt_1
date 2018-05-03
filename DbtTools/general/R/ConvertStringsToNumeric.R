ConvertStringsToNumeric=function(Strings,RelevantStrings,alphabeticOrder=TRUE){
  
  #author:MT 2018
  if(!missing(RelevantStrings)){
    ind=c()
    for(i in 1:length(RelevantStrings))
      ind=c(ind,which(Strings==RelevantStrings[i]))
    
    nonrelevant=setdiff(x=1:length(Strings),ind)
    Strings[nonrelevant]='NA'
  }
  Cls=rep(NaN,length(Strings))
  if(alphabeticOrder){
    u=sort(unique(Strings))
    n=length(u)
    for(i in 1:n)
      Cls[Strings==u[i]]=i
    # Cls=sapply(1:n,FUN = function(i,Cls,Strings,u) return( Cls[Strings==u[i]]=i),Cls,Strings,u)
    # Cls=as.numeric(sapply(X = as.character(1:n),FUN = function(i,Cls,u){Cls[Cls==u[i]]=i;return(Cls[Cls==i])},Cls,u))
    return(list(Cls=Cls,UniqueStringsFound=u,ConvertedTo=sort(unique(Cls))))
  }else{
    u=unique(Strings)
    n=length(u)
    for(i in 1:n)
      Cls[Strings==u[i]]=i
    # Cls=sapply(1:n,FUN = function(i,Cls,Strings,u) return( Cls[Strings==u[i]]=i),Cls,Strings,u)
    # Cls=as.numeric(sapply(X = as.character(1:n),FUN = function(i,Cls,u){Cls[Cls==u[i]]=i;return(Cls[Cls==i])},Cls,u))
    return(list(Cls=Cls,UniqueStringsFound=u,ConvertedTo=unique(Cls)))
  }
}