as.rda <- function(name){
  # Funktion die ein paar aus .cls und .lrn Dateien im Arbeitsverzeichnis 
  # in eine .rda konvertiert
  #
  ###INPUT
  # name    Name des Datensatzes. 
  #              (name).lrn, sowie (name).cls müssen im Arbeitsverzeichnis exisitieren
  ###OUTPUT
  # rda     Liste aus Matrix der Daten und Cls als Vektor. Entspricht der geschriebenen RDA Datei
  #
  # author: FP
  
  # LRN und Cls einlesen
  tmp <- ReadLrnCls(data = name)
  # Dimensionen in der Matrix umbennen (X1 ...  Xn statt C1 ... Cn)
  a = list()
  for(i in 1:dim(tmp$lrn$Data)[2]){
    a <- c(a, paste("X", as.character(i), sep = ""))
  }
  colnames(tmp$lrn$Data) <- a
  # Matrix und Cls aus tmp extrahieren und abspeichern
  # Damit das in R eigeladene Objekte hinterher den richtigen Namen hat
  # wird hier der Inhalt der Variable name als Variable genutzt.
  # Dadurch muss der Code erst zusammengesetzt und dann manuell geparsed werden.
  eval(parse(text = paste(name," = list(Data = tmp$lrn$Data, Cls = as.vector(tmp$cls$Cls))")))
  eval(parse(text = paste( "save(file = paste(name, \".rda\", sep = \"\"),"  , name, ")", sep = "" )))
  return(rda)
}

all.as.rda <- function(){
  # Nimmt alle *.lrn Dateien im Arbeitsverzeichnis und Versucht
  # as.rda für jede auszuführen
  for (i in dir()[grepl("*.lrn", dir())]){
    as.rda(substr(i, 1, nchar(i)-4))
  }
  
}