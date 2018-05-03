`relDiff4AB` <- function(a,b){

# function RelDiff = RelDiff4AB(A,B);
# % RelDiff = RelDiff4AB(A,B); % RelDiff = (B - A ) /( 0.5*(A+B))
# % RelDiff = RelDiff4AB(PreisGestern,PreisHeute);
# % Die RelativeDifferenz ist die auf den mittleren Grundwert bezogene ƒnderung.
# % INPUT
# % A,B(1:d,1:n)       Grundgroessen, A+B ~= 0 !
# % OUTPUT
# % RelDiff(1:d,1:n)   die auf den mittleren Grundwert bezogene ƒnderung:
# %                    RelDiff = (B - A ) /( 0.5*(A+B)); NaN f¸r A+B == 0 
# 
# % ALU Dez. 2007
# 
# Nenner = (A+B);
# NennerZeroInd = find(Nenner==0);
# if length(NennerZeroInd) >0 % alles == 0 kommt vor
#    Nenner(NennerZeroInd) = 1; % dies sorgt fuer RelDiff = 0, wenn alles 0 ist
# end; % if length(NennerZeroInd) >0
# 
# RelDiff = 2*(B - A )./ Nenner;
# 


	deno <- a+b;
	nZeroInd <- deno==0
	
	if(sum(nZeroInd)>0){
		deno[nZeroInd] =1
	}
	
	relDiff = 2*(b-a)/deno
	return(relDiff)

}