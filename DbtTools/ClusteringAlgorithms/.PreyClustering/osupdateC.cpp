#include "mex.h"
#include <math.h>
#include <stdlib.h>

#include "myfunctions.h"


//-------------------------------------------------------------------------
// CONVERT copies the value of each result matrices position to the output pointers
void CopyVectors(double *xout, double *x, int N)
{
    for(int i=0; i<N; i++)
      xout[i]=x[i];
}


//-------------------------------------------------------------------------
                       /*  GATEWAY FUNCITON */
//-------------------------------------------------------------------------
void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[])
{
 // ----------------------  INPUT VARIABLES -------------------------------
 //--- Create a pointer to the input matrix Grid and obtain dimensions
 double *Grid; int nrows,mcols;
 
 Grid  = mxGetPr(prhs[0]);
 nrows = mxGetM(prhs[0]); // Number of rows
 mcols = mxGetN(prhs[0]); // Number of columns
 
 //--- Create a pointer to the input matrix EmptySites and obtain dimension
 double  *OccupiedSites; int Nos;
 
 OccupiedSites = mxGetPr(prhs[1]);
 Nos           = mxGetN(prhs[1]); 
 
 if(mxGetM(prhs[1])!=1)
     mexErrMsgTxt("EmptySites must be a row vector.");
 
 
 //--- Create a pointer to the input matrix X and obtain dimension
 double  *DisMatrix; int Nx;
 
 DisMatrix  = mxGetPr(prhs[2]);
 Nx         = mxGetM(prhs[2]);
 
 //--- Get the scalar inputs: oi and MaxNeigbhbors
 int  MaxNeighbors;
 MaxNeighbors     = (int) mxGetScalar(prhs[3]);
 
 
 //--------------------------  MAIN ALGORITHMS  ---------------------------
 int oi, Nojs, posr, posc;
 double ojsAttrIndex[MAXSIZE], ojsGD[MAXSIZE];
 double *foi; foi = new double [Nos];

 for(int i=0; i<Nos; i++)
 {  
  oi = (int) Grid[(int)OccupiedSites[i]-1];
  
  // Convert columnwise matrix position into a coordinate
  single2coord((int) OccupiedSites[i],nrows,mcols,&posr,&posc);
  
  // Search nearest neighbors
  neighbors(Grid, nrows, mcols, posr, posc, MaxNeighbors, ojsAttrIndex, ojsGD, &Nojs);

  // Compute neighborhood similarity
  foi[i] = DensityC(oi,ojsAttrIndex, ojsGD, Nojs, DisMatrix, Nx);
 } 
 
 
 //------------------------- OUTPUT VARIABLES -----------------------------
 //  create a C pointer to a copy of the output matrix 
 double *f;
 plhs[0] = mxCreateDoubleMatrix(1,Nos, mxREAL);
 f       = mxGetPr(plhs[0]);
 
 // Copy the result matrices to the ouput pointers
 CopyVectors(f,foi,Nos);
}
