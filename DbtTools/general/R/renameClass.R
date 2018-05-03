renameClass=function(oldCls,oldName,newName) {
# Konsistentes Umbenennen von Klassen
# INPUT
# oldCls    Spaltenvektor der Klasssennamen (nummern)
# oldName   diese Klassennummer ist zu aendern 
# newName   in diesen 
# OUTPUT
# newCls    alle Vorkomnisse von oldName sind gegen newName ausgetauscht 
#           und alle Vorkomnisse von newName sind gegen oldName ausgetauscht
# reimplemented from matlab by MT 2016
  
  WoKommtAltVor = which(oldCls == oldName)
  WoKommtNeuVor = which(oldCls == newName)
  
  newCls = oldCls  # init, uebernahme der nicht zu aendernden
  newCls[WoKommtAltVor] = newName
  if (length(WoKommtNeuVor) > 0) {
    # der Neue Name kam schon mal vor
    newCls[WoKommtNeuVor] = oldName
  }  #length(WoKommtNeuVor) >0 # der Neue Name kam schon mal vor
  
  return(newCls)}