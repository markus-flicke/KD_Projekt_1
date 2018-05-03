#include <math.h>
#include <stdlib.h>
#include <new.h>


#define MAXSIZE 100
#define GRIDMODE 1

//-------------------------------------------------------------------------
// COORD2SINGLE converts coordinate (posr,posc) to its columnwise position
int coord2single(int posr, int posc, int nrows)
{ int i;
  i=posr + (nrows*(posc-1));
  return i;
}

// SINGLE2COORD converts a matrix columnwise position into its coordinate
void single2coord(int i, int Nrow, int Ncol, int *posr,int *posc)
{ // Row position
  *posr = i%Nrow;
  if(*posr==0) *posr=Nrow;

  // Column position
  *posc = (int)floor(1.0*i/Nrow)+ int((i%Nrow)/(*posr));
}

// FITPOS allows the grid to behave as a toroidal or fixed grid
int fitpos(int x,int M, bool select)
{   int xfit;
    switch(select)
    {   //Using a toroidal grid
        case 0:   
         if(x<=0)
            xfit = x+M;
         else
           { if(x>M) xfit = x-M;
             else    xfit=x;
           }
    
         if(xfit<=0 || xfit>M)  xfit = fitpos(xfit,M,0);
         break;
      
      // Using a fixed grid   
      case 1:
         if(x<=0 || x>M) xfit = -1;
         else            xfit=x;
         break;
    }
    return xfit;
}


//-------------------------------------------------------------------------
// CHESSBOARDD calculates the chess-board distance between points (x,y) and (s,t)
int ChessboardD(int x, int y, int s, int t)
{ int d;
  if(abs(x-s)>=abs(y-t)) d=abs(x-s);
  else                   d=abs(y-t);  
    
  return d;  
}

// EUCLIDEAND calculates the euclidean distance between points (x,y) and (s,t)
double EuclideanD(int x, int y, int s, int t)
{ double d;
  d=sqrt(pow(x-s,2)+pow(y-t,2));
    
  return d;  
}


// CITBLOCKD calculates the city-block distance between points (x,y) and (s,t)
int CityBlockD(int x, int y, int s, int t)
{ int d;
  d=abs(x-s)+abs(y-t);
    
  return d;  
}


//-------------------------------------------------------------------------
// NEIGHBORS searchs nearest occupied neighbors of site (posr, posc)
void neighbors(double *Grid, int nrows, int mcols, int posr, int posc, int MaxNeighbors,
               double *ojsAttrIndex,  double *ojsGD, int *Nojs)
{
 
 int radius = 0;          // Sweep radius
 int j=0;                 // Number of currently encountered neighbors
 int idx_rows, idx_cols;
 double result;

 
 while(j<MaxNeighbors)
  { radius++;
    for(int n=1; n<=2*radius+1; n=n+2*radius)  // Go through rows
        for(int m=1; m<=2*radius+1; m++)       // Go through columnss
          { // Row position
            idx_rows = fitpos(posr-radius+(n-1),nrows,GRIDMODE);

            // Column Position
            idx_cols = fitpos(posc-radius+(m-1),mcols,GRIDMODE);
            
            // Fill neighbors vector
            if (idx_rows!=-1 && idx_cols!=-1)
            { result = Grid[coord2single(idx_rows,idx_cols,nrows)-1];
              if(result!=0)
                { j=j+1;
                  ojsAttrIndex[j-1] = result;
                  //ojsGD[j-1]        = EuclideanD(posr,posc,idx_rows,idx_cols);
                  //ojsGD[j-1]       = CityBlockD(posr,posc,idx_rows,idx_cols);
                  ojsGD[j-1]       = radius;
                }
            }
          }

    for(int m=1; m<=2*radius+1; m=m+2*radius)       // Go through columns
        for(int n=2; n<=2*radius; n++)              // Go through rows
          {  // Row position
            idx_rows = fitpos(posr-radius+(n-1),nrows,GRIDMODE);

            // Column position
            idx_cols = fitpos(posc-radius+(m-1),mcols,GRIDMODE);
            
            if(idx_rows!=-1 && idx_cols!=-1)
            {  result = Grid[coord2single(idx_rows,idx_cols,nrows)-1];
              if(result!=0)
                { j=j+1;
                  ojsAttrIndex[j-1] = result;
                  //ojsGD[j-1]       = EuclideanD(posr,posc,idx_rows,idx_cols);
                  //ojsGD[j-1]       = CityBlockD(posr,posc,idx_rows,idx_cols);                  
                  ojsGD[j-1]       = radius;
                }
            }
        }
  }

 *Nojs = j;
}


//-------------------------------------------------------------------------
// PENALTYFUN calculates a penalization value for gd
double PenaltyFun(double gd)
{
  double p;
  // Penalization
  //p = 1./GD;

  //Exponential penalization
  p = exp(-0.2*(gd-1));

  return p;
}


// DISIMILARITY FUNCTION computes dissimilarity between x and y using DisMatrix
double DissimilarityFun(int x,int y, double *DisMatrix, int Nx)
{   int coord;
    double d;    

    coord=coord2single(x,y,Nx);
    d = DisMatrix[coord-1];
    return d;
}


double DensityC(double oi, double *ojsAttrIndex, double *ojsGD, int Nojs, double *DisMatrix, int Nx)
{
 double ds, gd, f;

 f=0;
 for(int i=0; i<Nojs; i++)
  { 
    // Dissimilarity function
    ds = DissimilarityFun((int)oi, (int)ojsAttrIndex[i], DisMatrix, Nx);

    // Penalty based on grid distance
    gd = PenaltyFun(ojsGD[i]);
    
    // Similarity density
    f=f + (1-ds)*gd;
  }
 f/=Nojs;
 
 return f;
}