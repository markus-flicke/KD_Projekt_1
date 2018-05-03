`Silhouette` <- function(X=NULL, clust=NULL, distance='sqEuclidean', showPlot=TRUE){
  

  
  #SILHOUETTE Silhouette plot for clustered data.
  #   SILHOUETTE(X, CLUST) plots cluster silhouettes for the N-by-P data
  #   matrix X, with clusters defined by CLUST.  Rows of X correspond to
  #   points, columns correspond to coordinates.  CLUST is a numeric vector
  #   with a common value for points in the same cluster.  SILHOUETTE treats
  #   NaNs, or empty strings, in CLUST as missing values, and ignores the
  #   corresponding rows of X.  By default, SILHOUETTE uses the squared
  #   Euclidean distance between points in X.
  #
  #   S = SILHOUETTE(X, CLUST) returns the silhouette values in the N-by-1
  #   vector S, and plot the cluster silhouettes.  
  #
  #   S = SILHOUETTE(X, CLUST, showPlot=FALSE) returns the silhouette values in the N-by-1
  #   vector S, but does not plot the cluster silhouettes.
  #
  #   S = SILHOUETTE(X, CLUST, DISTANCE) plots the silhouettes using
  #   the inter-point distance measure specified in DISTANCE.  Choices
  #   for DISTANCE are:
    #  
  #       'euclidean'     - Euclidean distance
  #      {'sqEuclidean'}  - Squared Euclidean distance
  #       'cityblock'     - Sum of absolute differences, a.k.a. L1
  #       'cosine'        - One minus the cosine of the included angle
  #                        between points (treated as vectors)
  #       'chebychev'     - 
  #       'jaccard'       - Percentage of non-zero coordinates that differ
  #       'minkowski'     - 
  #
  #
  #
  #   The silhouette value for each point is a measure of how similar that
  #   point is to points in its own cluster vs. points in other clusters,
  #   and ranges from -1 to +1.  It is defined as
  #
  #      S(i) = (min(AVGD_BETWEEN(i,k)) - AVGD_WITHIN(i))
  #                              / max(AVGD_WITHIN(i), min(AVGD_BETWEEN(i,k)))
  #
  #   where AVGD_WITHIN(i) is the average distance from the i-th point to the
  #   other points in its own cluster, and AVGD_BETWEEN(i,k) is the average
  #   distance from the i-th point to points in another cluster k.
  #
  #   Example:
    #
  #  
  #     X = rbind( cbind(rnorm(10,1,1)+1,rnorm(10,1,1)+1) , cbind(rnorm(10,1,1)-1,rnorm(10,1,1)-1) )
  #     cidx = kmeans(X, 2)
  #     clust = cidx$cluster
  #     s = Silhouette(X, clust)
  #
  #
  # Author Hansen-Goos 2015
  #
  
  
  
  if (is.null(X) || is.null(clust)){
    warning('notEnoughInputs');
  }

  
  
  
  nIn = length(clust)
  if (nIn != size(X)[1]){
    warning('stats:silhouette:InputSizeMismatch')
  }
  
                      #nans = which(is.nan(idx) & any(is.nan(X)));
                     
                      #nans = which(is.nan(idx))
                      #if (~isempty(nans)){
                      #  X <- X[-nans]
                      #  idx <- idx[-nans]
                      #}
  
  

  
  #  sorts a numeric grouping variable in ascending order (ersetzt [idx,cnames] = grp2idx(clust); in MAtlab)
  idx=clust
  temp=table(clust)
  cnames=names(temp)
  k = length(cnames);
  for (i in 1:k){
    idx[idx==str2num(cnames[i])] = i
  }
  
  # get size of the data
  n = length(idx);
  p = size(X)[2];
  
  # Get number of members of every Group
  count=rep(0,k)
  for (i in 1:k){
    count[i]=sum(idx==i)
  }
  
#  distance = 'sqeuclidean'
  
#  mbrs = matrix(0,n,k)
#  for (i in 1:n){
#    mbrs[i,idx[i]] = 1
#  }
  
  # Create a list of members for each cluster
  mbrs = matrix(0,n,k)
  mbrs[t(repmat(1:k,1,n)) == repmat(idx,1,k)] = 1
  
  # Get avg distance from every point to all (other) points in each cluster
  avgDWithin = matrix(Inf,n)
  avgDBetween = matrix(Inf,n,k) 
  distjAll = DistanceMatrix(X,method=distance)
  for (j in 1:n) {
                              #distj=sqrt(rowSums((X - t(repmat(X[j,],1,n)))^2))                             
                              #distj = dist2All(X[1,],X)
    distj=distjAll[j,]
       
    # Compute average distance by cluster number
    for (i in 1:k){
      if (i == idx[j]){
        avgDWithin[j] = sum(distj[mbrs[,i]==1]) / max(count[i]-1, 1)
      } else {
        avgDBetween[j,i] = sum(distj[mbrs[,i]==1]) / count[i]
      }
    }
  }

  #Calculate the silhouette values
  minavgDBetween = apply(avgDBetween,1,min)
  silh = (minavgDBetween - avgDWithin) / apply(cbind(avgDWithin,minavgDBetween),1,max)


  # Create the bars:  group silhouette values into clusters, sort values
  # within each cluster.  Concatenate all the bars together, separated by
  # empty bars.  Locate each tick midway through each group of bars
  space = max(floor(.02*n), 2)
  bars = matrix(Inf,space)

  tcks=0
  for (i in 1:k){
    bars = c(bars, -sort(na.last=T,-silh[idx == i]), matrix(Inf,space));
    tcks[i] = length(bars);
    end
  }
  tcks = tcks - 0.5*(diff(c(space,tcks)) + space - 1)
    
  # Plot the bars, don't clutter the plot if there are lots of
  # clusters or bars (>20)
  if (k > 20){
      cnames = ''
  }


  if (showPlot){
    barplot(rev(bars), width=1, space=0, ylim=c(1,length(bars)),xlim=c( min(c(bars,0)), 1.1 ), xlab="Silhouette Value", ylab="Cluster", col="blue4", border = NA, horiz=TRUE)
    axis(2 ,at=length(bars)-tcks, labels=cnames)
    box()
  }
     


  return(invisible(silh))
  
}
  
  
  
  
  
  
  
  
  
  
  
