`sloginvers` <-
function(signedLog,m=0){

# function  x = sloginvers(SignedLog,m);
# % x = sloginvers(SignedLog,m);
# % Inverse funktion zu slog: x == sloginvers(slog(x,m),m);
# % INPUT
# % SignedLog = sign(x) .* log(abs(x)+1) fuer LogBase =m, default m=10
# % m           Mantisse des Log, wenn =0 oder nicht angegebenn => ln
# % OUTPUT
# % x           Data 
# % ALU 2007
# 
# if nargin <2 ; m=0; end;
# AbsSL = abs(SignedLog);
# switch m
#     case 0 % natuerlicher log
#         x = sign(SignedLog).* (exp(AbsSL)  -1); %SignedLog = sign(x).* log1p(AbsSL); % log1p(x) = log(x+1) nur genauer
#     case 2
#         x = sign(SignedLog).* ( (2.^AbsSL) -1);% SignedLog = sign(x).* log2(AbsSL+1);
#     case 10
#         x = sign(SignedLog).* ((10.^AbsSL) -1);% SignedLog = sign(x).* log10(AbsSL+1);
#     otherwise % andere Basen 
#         x = sign(SignedLog).* ( (m.^AbsSL) -1);        
# end; % switch
# 
# 


	absSL = abs(signedLog)
	s = sign(signedLog)

	if(m==0){
	 	return(s* exp(absSL))
	}
	
	if(m==2){
		return(s* ((2^absSL)-1))
	}
	
	if(m==10){
		return(s*(10^absSL)-1)	
	}
	else {
	factor = log(m)
	return(s*(m^absSL)-1)
		
	}



 }

