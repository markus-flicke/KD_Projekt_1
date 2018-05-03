waitbar <- function(ProgressInPercent,Header = "R progress bar",Handle=c()){
# Tacho <- waitbar(0,'R progress bar')             # erzeugen eines Fortschrittsbalkens
# Tacho <- waitbar(ProgressInPercent,Header,Tacho) # weiterschalten des Fortschrittsbalkens
# close(Tacho)                                     # Fortschrittsbalken schliessen

#MT: Funktioniert angeblich nicht unter linux
if((Sys.info()["sysname"]=="Windows")){
	requireNamespace('utils')
  if ( ProgressInPercent<=0){
     Handle <- utils::winProgressBar(title = Header, label = "", min = 0, max = 1, initial = 0, width = 300);
  }else{
     utils::setWinProgressBar(Handle, ProgressInPercent/100, title = Header)  ;
  } # end if
}
return(Handle);
} # end function waitbar