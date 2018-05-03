`tileplot` <- function(numOfRows, numOfCols, i = numOfRows*numOfCols){

# function tileplot(Rows,Cols,i);
# %  tileplot(Rows,Cols,i);
# % do subbplots, ito different windows if necessary
# 
# TileNr =mod(i, Rows*Cols); 
# if TileNr ==0; TileNr = Rows*Cols; drawnow; end;
# if TileNr ==1 figure;  end;
# subplot(Rows,Cols,TileNr);


TileNr <- i%%(numOfRows*numOfCols)
if(TileNr == 1){
	par(mfrow=c(numOfRows, numOfCols))
}
if(TileNr == 0 & numOfRows*numOfCols == 1){
	par(mfrow=c(numOfRows, numOfCols))
}
 
}