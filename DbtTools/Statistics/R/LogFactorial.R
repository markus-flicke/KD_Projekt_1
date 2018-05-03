LogFactorial <- function(nIn){
# LogFac = LogFactorial(n)
# LogFactorial returns the (natural) logarithm of N!.
# with   N! = Product ( 1 <= I <= N ) 
# INPUT
# nIn[1:k]    input to facultaet
#
# OUTPUT
#  LogFac[1:k]  the logarithm of N! LogFac = log(n!)
#  LogFac ==nan for n<0
# 
#  Author: John Burkardt
#  Source: http://people.sc.fsu.edu/~jburkardt/m_src/prob/prob.html
#  modifications by ALU transferded to dbt by MT:
#  dbt documentation 
#  LogFac == nan for n<0
#  vectorisiert    
# Edit: CL fuer n ueber 1 und n ueber 0 richtige Werte!

n = as.vector(nIn)

if (length(n) <2){ #n ist nur eine Zahl
  if ( n < 0 ){
      LogFac = NA
  }else{
		if(n == 0){
			LogFac = 0
		}else{		
			i= c(1:n)
			LogFac = sum(log(i))
		}# if (n == 0)
  }# if ( n < 0 )
}else{ # n is a vector
      LogFac <- rep(NA, length(n)) # init Output
      PosInd = which(n >=0 )
			Nuller <- which(n == 0)
			if(length(PosInd)!=0){ # wenn es nur negative gibt, tue nichts (alles NA), sonst for-Schleife
      for (k in 1:length(PosInd)){
          Ind =  PosInd[k]
					if(Ind %in% Nuller){
						LogFac[Ind] <- 0
					}else{
						LogFac[Ind] = sum(log(t(c(1:n[Ind]))))
					}
      } # for k
			}# end if(length(PosInd)!=0) 
}# if length(n)

return(LogFac)
}