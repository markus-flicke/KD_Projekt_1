plot3 <-
function (X,Y,Z,LineSpec='b.',LineWidth=1,MarkerSize=1)
{
# library('rgl')
requireNamespace('rgl')
    V= LineSpec2R(LineSpec)       # umcodierung der LineSpec zu R werten
    Ltype=V[1]; Lpch=V[2];Lcol=V[3];Llty=V[4];
    if (Lpch=='.'){
        rgl::plot3d(x, y, z, col=Lcol)
    }
    else {
       rgl::plot3d(x, y, z, col=Lcol)
    } # if 
}  #END FUNCTION

