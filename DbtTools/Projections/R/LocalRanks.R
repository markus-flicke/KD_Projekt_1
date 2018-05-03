LocalRanks= function(Ind,Distances,AnzInd = length(which(is.finite(Ind)))){
  # Ranks = LocalRanks(Ind,Distances,AnzInd);
  # die Raenge einer Indexmenge in einer Distanzmatrix ermitteln
  #
  # INPUT
  # Ind(1:n,1:k)                        Ind(i,:) sind k indizes in Distance(1:n,1:n) oder NaN
  # Distance(1:n2) or Distance(1:n,1:n) Matrix of Distances mit Distance(1:n2) == squareform(Distance(1:n,1:n))
  #
  # OPTIONAL
  # AnzInd(1:n)                         AnzInd(i) ist die Anzahl der gueltigen Eintraege in Ind(i,:)
  #                                     wenn nicht angegeben  AnzInd = sum(isfinite(Ind),2);
  #
  #
  # OUTPUT
  # Ranks(1:n,1:k)                      Ranks(i,:) sind die Raenge der Werte von Ind(i,:) in Distance(i,1:n)
  # AnzRanks(1:n)                       AnzRank(i) ist die Anzahl der gueltigen Eintraege in  Ranks(i,:)
  #
  # author: reimplemented from ALU?s matlab version by MT
  #
  # Nota
  # matlabsum(is.finite(Ind)) feststellen der Anzahl indices
  #
  # zu quadratischer matrix machen und sortieren
 
  if (is.vector(Distances)) {
    requireNamespace("pracma")
    #falls Vektor, wieder zu quadratischer Matrix machen
    Distances = pracma::squareform(Distances)
  }# [l,c]
  #SortedDists = -apply(-Distances, 2, sort, decreasing = T)
  Sind = apply(-Distances, 2, order, decreasing = T)
  AnzCases = ncol(Distances)
  # Sind = Sind(2:end,:); % die oberste Zeile weglassen, da es den Punkt selbst bezeichnet Alternative 1 abziehen von den RankInSind
  #
  # Sind enthaelt jetzt die Positionen der Distanzen, der Groesse nach aufsteigend geordnet => Rangreihenfolge
  
  Ranks = Ind * NA               # init, nan sind die nicht gebrauchten
  AnzRanks = matrix(0, AnzCases, 1) #init zu Null
  
  for (i in 1:AnzCases) {
    # matlab: [CommonInd, RankInSind] = intersect(Sind(:,i),Ind(i,1:AnzInd(i))');
    CommonInd = intersect(Sind[, i], Ind[i, 1:AnzInd[i]])
    #RankInSind=match(CommonInd,Sind[,i]) #liefert nur 1-6
    RankInSind = which(Sind[, i]  %in% Ind[i, 1:AnzInd[i]]) #liefert nur 1-6
    AnzCommon = sum(!is.na(RankInSind)) #length(RankInSind)
    if (AnzCommon > 0) {
      Ranks[i, 1:AnzCommon] = t(RankInSind - 1) # minus 1 wegen 1. Zeile ?????
    }  #if AnzCommon >0
    AnzRanks[i] = AnzCommon
  } # for i=1:AnzCases
  return(list(Ranks = Ranks, AnzRanks = AnzRanks))
}