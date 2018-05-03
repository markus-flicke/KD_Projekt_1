randomLogMix <- function(M,S,W,IsLogDistribution,TotalNrOfPoints){
  # Mix = randomLogMix(M,S,W,IsLogDistribution,TotalNrOfPoints);
  # genereierung von ZufalsDaten, die einer Mischung von Vereilungen aus
  # Gauss & Log-Normalen folgt
  # INPUT
  # x     zu zeichnende Vatiable
  # M(1:L), S(1:L), W(1:L) die Paramter von der Verteilungen
  # IsLogDistribution(1:L) gibt an ob die einzelverteilung einer (generalisierten)Lognormaverteilung ist
  #             wenn IsLogDistribution(i)==0 dann Mix(i) = W(i) * N(M(i),S(i)
  #             wenn IsLogDistribution(i)==1 dann Mix(i) = W(i) * LogNormal(M(i),S(i)
  # Die Gesamtverteilung ergibst sich als Summe der Mix(1:L)
  # 
  # OPTIONAL
  # TotalNrOfPoints              Mix enthalet so viele Punkte am Schluss.
  #                              Default: TotalNrOfPoints ca 1000
  #
  # NOTA: die Log-Normalverteilung ist generaisiert d.h.: L(M,S) = sign(M)*lognrnd(abs(M),S);
  
  
  
  L= length(M)                 # Anzahl Mischungen
  
  #if nargin > 4 # TotalNrOfPoints gegeben
  #AnzPoints = round(W*TotalNrOfPoints*1.1); 
  # wegen Rundungsfehlern kann dies mehr oder weniger als die angegebene # Anz sein.
  
  if(missing(TotalNrOfPoints)){
    TotalNrOfPoints = 1000
    AnzPoints = round(W* TotalNrOfPoints/sum(W))    # Anzahl Punkte die pro Mischung generiert werden
    MinInd = which.min(AnzPoints) # rundung bereinigen
    MinPoints = AnzPoints[MinInd]  
    OtherInd = which(1:L!=MinInd)
    AnzPoints[MinInd] =  TotalNrOfPoints-sum(AnzPoints[OtherInd])
  }else{
    AnzPoints = round(W*TotalNrOfPoints*1.1)
  }
  
  
  # Verteilung der Mischungen  erzeugen
  Mix = c() # init
  for(d in 1:L){
    if(IsLogDistribution[d]==1){
      Mixi = symlogrnd(M[d],S[d],AnzPoints[d],1) # Mix(i) als LogNormal erzeugen
    }else{ # Mormalverteilung
      Mixi = normrnd(M[d],S[d],AnzPoints[d],1) # Mix(i) als Gauss erzeugen
    }
    Mix = c(Mix,Mixi)
    #     subplot(1,2,1);histopt(Mixi); subplot(1,2,2);histopt(Mix);
    #     pause;
  }  # for d
  # hier enthaelt Mix die der Vereilung entsprechende Punkte
  
  # Dureinanderwuerfeln und auf gewuenschte Anzahl bringen
  Ind = randperm(TotalNrOfPoints)
  #Ind = min(Ind,length(Mix)) # ggf auffuellen oder kuerzen
  Mix=Mix[Ind]
  # histopt(Mix);
  
  return(Mix)
  
}