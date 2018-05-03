sortdescending <- function(x){

# function [Y,I] = sortdescending(X);
# % [Y,I] = sortdescending(X);
# % Sort in descending order. ansonsten wie sort.
# % Author ALU
# [Y,I] = sort(na.last=T,X,'descend');
  if(class(x)=="matrix"){
    tmp <- apply(x,2,sort,decreasing=TRUE,index.return=TRUE)
    tmp <- unlist(tmp,recursive=FALSE,use.names=FALSE) # Flatten the list.
    size <- c(dim(x),length(tmp))
    # Create matrices out of tmp. x = sorted matrix, ind = sorted indices.
    x <- matrix(unlist(tmp[seq(1,size[3],2)]),size[1],size[2])
    ind <- matrix(unlist(tmp[seq(2,size[3],2)]),size[1],size[2])
    return(list("sort"=x,indices=ind))
  }else
    result <- (sort(na.last=T,x,decreasing=TRUE,index.return=TRUE))
    names(result) <- c("sort","indices")
    return(result)
}