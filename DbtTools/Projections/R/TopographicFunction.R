TopographicFunction <- function(Data, BM, k=0) {
# phi <- TopographicFunction(Data, BM, k)
# The topographic function calculates a phi-value, which is a measure for the
# degree of topology preservation of an SO(F)M projection as proposed by Thomas
# Villmann et al.[1] The SOFM of M formed by A is defined by
# (Phi_{A->M}, Phi_{M->A}). Phi_{M->A} is the mapping of points from input
# space M into lattice A, and Phi_{A->M} is the inverse mapping from A to M.
# The mapping Phi_{M->A} is neighbourhood preserving iff locations w_i, w_j
# which are adjacent on M belong to vertices i,j which are adjacent in A
# according to the maximum-norm on A.
# THe mapping Phi_{A->M} is neighbourhood preserving iff vertices i, j which
# are adjacent in A accourding to summation-norm on A are assigned to
# neighbouring locations w_i, w_j in M.
# w_i, w_j are adjacent in M iff their receptive fields determined by the
# masked Voronoi polyhedra are adjacent. This is calculated by using a
# delaunay triangulation from geometry::delaunayn.
# 
# INPUT
#   Data[1:n,1:m]   Point coordinates in input space M. These should have been
#                  used to calculate the SOFM. Hence their indices should
#                  correspond to the BMkeys in BMs.
#   BM             List of information about best matches read from a .bm-File
#                  using DataIO::ReadBM
#   k              Integer value. If k > 0: neighbourhood preservation of all
#                  points in lattice for mapping Phi_{M->A} is being
#                  calculated (.posTop), if k < 1: neighbourhood preservation
#                  of all points in lattice for mapping Phi_{A->M} gets
#                  calculated (.negTop). If k == 0:
#                  topographicFunction(1) + topographicFunction(-1) gets
#                  calculated.
# 
# OUTPUT
#   phi            Positive integer indicating degree of topology preservation.
#                  phi == 0 iff projection is perfectly topology preserving.
# 
# author:     schuster@mathematik.uni-marburg.de
# 
# EXAMPLE
#   hepta    <- ReadLRN('Hepta.lrn')$Data
#   hepta_bm <- ReadBM('Hepta_50x82e20.bm')
#   k        <- 0
#   phi <- topographicFunction(hepta, hepta_bm, k)
# 
# LITERATURE
#   [1] Thomas Villmann, Ralf Der, Michael Herrmann, and Thomas M. Martinetz,
#   "Topology Preservation in Self-Organizing Feature Maps: Exact Definition
#   and Measurement," in IEEE Transactions on Neural Networks,
#   vol. 8, no.2, MARCH 1997.

#  requireRpackage('geometry') #graph.adjacency
  
  BM$BestMatches=data.frame(BM$BestMatches)
  # extract BMs
  if (is.element('BestMatches', names(BM))) {
    BMs <- BM$BestMatches
  } else {
    BMs <- BM
  }
  if (is.element('Rows', names(BM))) {
    rows <- BM$Rows
  } else {
    # guess size
    rows <- max(BM$BMLineCoords)
  }
  if (is.element('Columns', names(BM))) {
    cols <- BM$Columns
  } else {
    # guess size
    cols <- max(BM$BMColCoords)
  }

  # construct adjacency matrix using delaunay triangulation
  adjM = delaunayAdjMatrix(Data)

  # get number of neural units
  N <- rows * cols

  count <- 0

  if (k > 0) {
    for (i in BMs$BMkey) {
      count <- count + .posTop(i, k, adjM, BMs)
    }
    return((1/N) * count)
  }

  else if (k < 0) {
    n=nrow(adjM)
    requireNamespace("GraphAlgorithms")
    paths <- GraphAlgorithms::KirkDijkstra(adjM, adjM, c(1:n), c(1:n), 0)
    for (i in BMs$BMkey) {
      count <- count + .negTop(i, -k, paths, BM)
    }
    return((1/N) * count)
  }

  else {# (k == 0)
    return(TopographicFunction(Data, BM,  1) +
           TopographicFunction(Data, BM, -1)
    )
  }
}

.posTop <- function(i, k, AdjM, BMs) {
# phi <- .posTop(i, k, AdjM, BMs)
# Helper function calculates the degree of neighbourhood preservation for
# mapping Phi_{M->A} of point i from input space M into lattice A.
# 
# INPUT
#   i              BMkey of neural unit
#   k              Positive integer >= 1 "radius of neighbourhood"
#   AdjM[1:n,1:n]  Adjacency matrix of points in input space M
#   BMs[1:n,1:3]   Bestmatches = [BMkey, BMLineCoords, BMColCoords]
# 
# OUTPUT
#   phi            Positive integer indicating degree of neighbourhood 
#                  preservation. phi == 0 iff mapping is perfectly
#                  neighbourhood preserving.
# 
# AUTHOR
#   schuster@mathematik.uni-marburg.de
# 
# EXAMPLE
#   hepta     <- ReadLRN('Hepta.lrn')$Data
#   hepta_bms <- ReadBM('Hepta_50x82e20.bm')$Data
#   hepta_am  <- delaunayAdjMatrix(hepta)
#   i         <- sample(hepta_bms$BMkey, 1)
#   k         <- 43
#   phi <- .posTop(i, k, hepta_am, hepta_bms)

  result <- 0
  for (j in 1:length(AdjM[i,])) {
    if (AdjM[i,j] == 1) {
      if (.maxNorm(BMs, i, j) > k) {
        result <- result + 1
      }
    }
  }
  return(result)
}

.negTop <- function(i, k, paths, BM) {
# phi <- .negTop(i, k, AdjGraph, BM)
# Helper function calculates the degree of neighbourhood preservation for
# inverse mapping Phi_{A->M} of point i from lattice A into input space M.
# 
# INPUT
#   i              BMkey of neural unit
#   k              Positive integer >= 1 "radius of neighbourhood"
#   AdjGraph       Graph of points in input space M
#   BM             List of information about best matches read from a .bm-File
#                  using DataIO::ReadBM
# 
# OUTPUT
#   phi            Positive integer indicating degree of neighbourhood 
#                  preservation. phi == 0 iff mapping is perfectly
#                  neighbourhood preserving.
# 
# AUTHOR
#   schuster@mathematik.uni-marburg.de
# 
# EXAMPLE
#   hepta     <- ReadLRN('Hepta.lrn')$Data
#   hepta_am  <- delaunayAdjMatrix(hepta)
#   hepta_g   <- graph.adjacency(hepta_am)
#   i         <- sample(hepta_bm$Data$BMkey, 1)
#   k         <- 43
#   phi <- .negTop(i, k, hepta_g, hepta_bm)

  result <- 0
  neighbours <- bmNeighbours(BM, i)
  for (j in neighbours) {
    path <- unlist(paths$paths[i, j])
    # path <- get.shortest.paths(AdjGraph, i, j)[[1]]
    if (length(path) - 1 > k) {
      result <- result + 1
    }
  }
  return(result)
}

delaunayAdjMatrix <- function(Data) {
# adjMat <- delaunayAdjMatrix(Data)
# This function generates an adjacency matrix of the supplied Data points by
# connecting the simplices calculated by the delaunay triangulation from
# geometry::delaunayn.
# 
# INPUT
#   Data[1:n,1:m]    Point coordinates in input space M.
# 
# OUTPUT
#   adjMat[1:n,1:n]  Adjacency matrix of Data calculated from delaunay
#                    triangulation from geometry::delaunayn
# 
# AUTHOR
#   schuster@mathematik.uni-marburg.de
# 
# EXAMPLE
#   hepta <- ReadLRN('Hepta.lrn')$Data
#   hepta_adjMat <- delaunayAdjMatrix(hepta)
# 
# uses package geometry

 # if(!require(geometry)) { # geometry::delaunayn
  #  install.packages('geometry')
#    library(geometry)
 # } else {
 #   library(geometry)
 # }
  # prepare N x N matrix filled with zeroes, where N = #{Data points}
  result <- matrix(0, nrow(Data), nrow(Data))

  # get simplices of connected points
  del <- delaunayn(Data)

  # apply takes *forever*
  for (r in 1:nrow(del)) {
             # get all pairwise combinations
    pairs <- combn(del[r,], 2)
    for (i in 1:ncol(pairs)) {
      result[pairs[1,i], pairs[2,i]] <- 1
      # edges go both ways
      result[pairs[2,i], pairs[1,i]] <- 1
    }
  }
  return(result)
}

bmNeighbours <- function(BM, i) {
# neighbours <- bmNeighbours(BM, i)
# Function returns the immediate neighbours of neutral unit i. Units on the
# edge of the lattice have neighbours on the opposite side.
# Exact size of lattice is (hopefully) supplied by BM, if not it's guessed by
# using max values of best matches Line- and ColCoords.
# 
# INPUT
#   BM             List of information about best matches read from a .bm-File
#                  using DataIO::ReadBM
#   i              key of best match within BM
# 
# OUTPUT
#   neighbours     List if keys that are immediate neighbours if point i
# 
# AUTHOR
#   schuster@mathematik.uni-marburg.de
# 
# EXAMPLE
#   hepta_bm <- ReadBM(Hepta_50x82e20.bm')
#   i        <- sample(hepta_bm$Data$BMkey, 1)
#   neighbours <- bmNeighbours(hepta_bm, i)

  if (is.element('BestMatches', names(BM))) {
    BMs <- BM$BestMatches
  } else {
    BMs <- BM
  }
  if (is.element('Rows', names(BM))) {
    r <- BM$Rows
  } else {
    # guess size
    r <- max(BM$BMLineCoords)
  }
  if (is.element('Columns', names(BM))) {
    c <- BM$Columns
  } else {
    # guess size
    c <- max(BM$BMColCoords)
  }

  result <- integer()

  unit = BMs[BMs$BMkey == i,]

  tmp <- BMs[BMs$BMLineCoords == unit$BMLineCoords,]

  # if multiple points on the same neural unit...
  result <- c(result, tmp$BMkey[tmp$BMColCoords == unit$BMColCoords])

  # above and below
  #   if neural unit is on an edge .mod() finds neighbour on the opposite side 
  # of matrix
  result <- c(result, tmp$BMkey[tmp$BMColCoords == .mod(unit$BMColCoords+1, c)])
  result <- c(result, tmp$BMkey[tmp$BMColCoords == .mod(unit$BMColCoords-1, c)])

  tmp <- BMs[BMs$BMColCoords == unit$BMColCoords,]

  # left and right
  result <- c(result, tmp$BMkey[tmp$BMLineCoords == .mod(unit$BMLineCoords+1, r)])
  result <- c(result, tmp$BMkey[tmp$BMLineCoords == .mod(unit$BMLineCoords-1, r)])

  return(result)
}

.sumNorm <- function(BMs, p1, p2) {
  j1 = BMs[BMs$BMkey == p1, -1]
  j2 = BMs[BMs$BMkey == p2, -1]

  result <- 0

  for (i in 1:length(j1)) {
    result = result + abs(j1[[i]] - j2[[i]])
  }
  return(result)
}

.maxNorm <- function(BMs, p1, p2) {
  j1 = BMs[BMs$BMkey == p1, -1]
  j2 = BMs[BMs$BMkey == p2, -1]

  result <- integer()

  for (i in 1:length(j1)) {
    result = c(result, abs(j1[[i]] - j2[[i]]))
  }
  return(max(result))
}

.mod <- function(x, m) {
  if (x == 0) {
    return(m)
  } else {
    return(x %% m)
  }
}

.bmm <- function(BM) {
  if (is.element('BestMatches', names(BM))) {
    BMs <- BM$BestMatches
  } else {
    BMs <- BM
  }
  if (is.element('Rows', names(BM))) {
    r <- BM$Rows
  } else {
    # guess size
    r <- max(BM$BMLineCoords)
  }
  if (is.element('Columns', names(BM))) {
    c <- BM$Columns
  } else {
    # guess size
    c <- max(BM$BMColCoords)
  }
  result <- matrix(-1, r, c)

  for (i in 1:nrow(BMs)) {
    result[BMs[i,]$BMLineCoords, BMs[i,]$BMColCoords] <- BMs[i,]$BMkey
  }
  return(result)
}










