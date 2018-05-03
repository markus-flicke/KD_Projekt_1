GetVariable=function(Data,Names,Header){
  if(!is.matrix(Data)){
    warning('Data is not Matrix. Calling as.matrix()')  
    Data=as.matrix(Data)
  }
  if(missing(Header)){
    Header=colnames(Data)
  }
  
  if(is.null(Header)){
    stop('No Header given or found in colnames of Data.')
  }
  
  Names=paste0('\\<',Names,'\\>')
  if(length(Names)==1)
    ColNum <- grep(paste0(Names),Header)
  if(length(Names)>1)
    ColNum=sapply(Names,grep,Header,ignore.case=FALSE)
  
  if(length(ColNum)==0)
    warning('Column name not found. Returnung empty vector of Data length.')
  
  return(Data[,ColNum])
  #author: MT 2018
}