symlognSigmaMue <-  function(M,S){

  variance<-log(S*S/(M*M)+1)
  sig<-sqrt(variance)
  mu<-log(abs(M))-0.5*variance
  return (list(variance=variance,sig=sig,mu=mu)) 

 }

