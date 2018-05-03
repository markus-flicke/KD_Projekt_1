\name{TopographicFunction}
\alias{TopographicFunction}
\title{ The topographic function calculates a phi-value, which is a measure for the degree of topology preservation of an SO(F)M projection as proposed by Thomas Villmann et al.[1] The SOFM of M formed by A is defined by (Phi_{A->M}, Phi_{M->A}). Phi_{M->A} is the mapping of points from input space M into lattice A, and Phi_{A->M} is the inverse mapping from A to M. The mapping Phi_{M->A} is neighbourhood preserving iff locations w_i, w_j which are adjacent on M belong to vertices i,j which are adjacent in A according to the maximum-norm on A. THe mapping Phi_{A->M} is neighbourhood preserving iff vertices i, j which are adjacent in A accourding to summation-norm on A are assigned to neighbouring locations w_i, w_j in M. w_i, w_j are adjacent in M iff their receptive fields determined by the masked Voronoi polyhedra are adjacent. This is calculated by using a delaunay triangulation from geometry::delaunayn. }
\arguments{\item{data[1:n,1:m]}{ Point coordinates in input space M. These should have been
used to calculate the SOFM. Hence their indices should
correspond to the BMkeys in BMs.}
\item{BM}{List of information about best matches read from a .bm-File
using DataIO::ReadBM}
\item{k}{Integer value. If k > 0: neighbourhood preservation of all
points in lattice for mapping Phi_{M->A} is being
calculated (.posTop), if k < 1: neighbourhood preservation
of all points in lattice for mapping Phi_{A->M} gets
calculated (.negTop). If k == 0:
topographicFunction(1) + topographicFunction(-1) gets
calculated.}
}
\value{phi            Positive integer indicating degree of topology preservation.
phi == 0 iff projection is perfectly topology preserving.}
\description{
 The topographic function calculates a phi-value, which is a measure for the degree of topology preservation of an SO(F)M projection as proposed by Thomas Villmann et al.[1] The SOFM of M formed by A is defined by (Phi_{A->M}, Phi_{M->A}). Phi_{M->A} is the mapping of points from input space M into lattice A, and Phi_{A->M} is the inverse mapping from A to M. The mapping Phi_{M->A} is neighbourhood preserving iff locations w_i, w_j which are adjacent on M belong to vertices i,j which are adjacent in A according to the maximum-norm on A. THe mapping Phi_{A->M} is neighbourhood preserving iff vertices i, j which are adjacent in A accourding to summation-norm on A are assigned to neighbouring locations w_i, w_j in M. w_i, w_j are adjacent in M iff their receptive fields determined by the masked Voronoi polyhedra are adjacent. This is calculated by using a delaunay triangulation from geometry::delaunayn. }
\author{schuster@mathematik.uni-marburg.de
}
