dbtplot <-
function (X,Y,LineSpec='b.',LineWidth=1,MarkerSize=1)
{
V= LineSpec2R(LineSpec)       # umcodierung der LineSpec zu R werten
Ltype=V[1]; Lpch=V[2];Lcol=V[3];Llty=as.numeric(V[4]);
print(V)

if(dev.cur()==1) #wenn noch keine Fenster offen ist
  {
  plot(X,Y,type=Ltype,pch=Lpch,col=Lcol,lty=Llty,xlab=' ',ylab=' ',cex=3)
  }
else  #wenn schon ein fenster offen ist.
  {    
  points(X,Y,type=Ltype,pch=Lpch,col=Lcol,lty=Llty,xlab=' ',ylab=' ',cex=3)      
  }
}  #END FUNCTION

