#include "mex.h"
#include <math.h>
#include <stdlib.h>

#include "myfunctions.h"


//-------------------------------------------------------------------------
// CONVERT copies the value of each result matrices position to the output pointers
void CopyVectors(double *xout, double *x, int Nes)
{
    for(int i=0; i<Nes; i++)
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
 double  *EmptySites; int Nes;
 
 EmptySites = mxGetPr(prhs[1]);
 Nes        = mxGetN(prhs[1]); 
 
 if(mxGetM(prhs[1])!=1)
     mexErrMsgTxt("EmptySites must be a row vector.");
 
 
 //--- Create a pointer to the dissimilarity matrix and obtain its size
 double  *DisMatrix; int Nx;
 
 DisMatrix = mxGetPr(prhs[2]);
 Nx        = mxGetM(prhs[2]);
 
 
 //--- Get the scalar inputs: oi, MaxNeigbhbors
 int  oi, MaxNeighbors;
 oi              = (int) mxGetScalar(prhs[3]);
 MaxNeighbors    = (int) mxGetScalar(prhs[4]);
 
 

 //--------------------------  MAIN ALGORITHMS  ---------------------------
 int Nojs, posr, posc;
 double ojsAttrIndex[MAXSIZE], ojsGD[MAXSIZE];
 double *foi; foi = new double [Nes];

 for(int i=0; i<Nes; i++)
 {  
  // Convert columnwise matrix position into a coordinate
  single2coord((int) EmptySites[i],nrows,mcols,&posr,&posc);

  // Search nearest neighbors
  neighbors(Grid, nrows, mcols, posr, posc, MaxNeighbors, ojsAttrIndex, ojsGD, &Nojs);

  // Compute neighborhood similarity
  foi[i] = DensityC(oi,ojsAttrIndex, ojsGD, Nojs, DisMatrix, Nx);
 } 
 
 
 //------------------------- OUTPUT VARIABLES -----------------------------
 //  create a C pointer to a copy of the output matrix 
 double *f;
 plhs[0] = mxCreateDoubleMatrix(1,Nes, mxREAL);
 f       = mxGetPr(plhs[0]);
 
 // Copy the result matrices to the ouput pointers
 CopyVectors(f,foi,Nes);
 delete [] foi;
}
