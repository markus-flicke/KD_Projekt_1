RAAR=function(Data,ProjectedPoints,kmax=nrow(Data)-2,PlotIt=T){
  #res=RAAR(Data,ProjectedPoints,PlotIt=T)
  #rescaled average agrrement rate deduced by the co ranking matrix from LCMC
  #INPUT
  # Data[1:n,1:d]      array of data: n cases in rows, d variables in columns, matrix is not symmetric
  #                           or distance matrix, in this case matrix has to be symmetric
  # ProjectedPoints[1:n,OutputDimension]               n by OutputDimension matrix containing coordinates of the Projection:
  #                                           Note: No Key before ProjectedPoints!
  # kmax                                    maximum of intervall 1:kmax of k nearest neighbors
  # PlotIt
  #OUTPUT
  # Raar[1:kmax]                                  rescaled average agreement rate
  # Aar[1:kmax]                                   average agreement rate
  #
  #author: MT 01/2016
  #Note: Lee, J. A., Peluffo-Ordonez, D. H., & Verleysen, M. Multiscale stochastic neighbor embedding:
  # Towards parameter-free dimensionality reduction. Paper presented at the Proceedings of 22st European
  # Symposium on Artificial Neural Networks, Computational Intelligence And Machine Learning (ESANN) (2014).
  
  
  # Input:
  # data: Input Data - Distance Matrix
  # i: An integer, who's number of local overlaps is sought
  # k: An integer, giving the number of nearest neighbors to i
  # proj: Output Data - Distance Matrix
  
  # Local Overlap
  requireNamespace("Distances")
  InputDistances = Distances::DistanceMatrix(Data)
  OutputDistances = Distances::DistanceMatrix(ProjectedPoints)
  aar = vector() #average agreement rate
  raar = vector() #average agreement rate
  N = nrow(Data)
  for (k in 1:kmax) {
    Hin = knneighborDistances(k, InputDistances)$NNind
    Hout = knneighborDistances(k, OutputDistances)$NNind
    overlappingInd = lapply(
      1:N,
      FUN = function(i, Hin, Hout) {
        return(intersect(Hin[i, ], Hout[i, ]))
      },
      Hin,
      Hout
    )
    lengthOfOverlapps = sapply(overlappingInd, length)
    aar[k] = sum(lengthOfOverlapps) / (k * N) #Q_nx(K)
    raar[k] = ((N - 1) * aar[k] - k) / (N - 1 - k) #R_nx(K)
  }
  if (PlotIt) {
    plot(
      1:kmax,
      100 * raar,
      type = 'l',
      log = "x",
      ylab = '100Rnx(K)',
      xlab = 'K',
      main = 'ROC',
      xaxs = 'i',
      las = 1,
      ylim = c(0, 100)
    )
  }
  return(list(Raar = raar, Aar = aar))
}