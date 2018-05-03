bar <-
function (X,Y=0)
{
  if (Y == 0) {
     barplot(X)}
  else  {
    hist(Y,breaks = X)
    } # if 
}  #END FUNCTION

