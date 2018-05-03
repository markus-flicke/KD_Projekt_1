`BackPropProjection` <-
function(Data,MLPBP){

erg <- compute(MLPBP,Data)
proS <- MLPBP$projektionsSchicht
return(erg$neurons[[1+proS]][,c(2,3)])


# function  ProjectedData = BackPropProjection(Data,MLPBP);
# % [ProjectedData] = BackPropProjection(Data,MLPBP);
# %  ein MLPBP projektion N -> k -> N
# % das Netz wiurde auf Identitaet trainiert, siehe Funktion
# % BackPropProjection(...)
# % INPUT
# % Data(1:n,1:d)          cases in rows, variables in columns
# % MLPBP                  das MLP mit Back-Propagation wie in den Mathlabfunktionen
# % OUTPUT
# % ProjectedData           die Projektion = Aktivierung der mittlersten Schicht
# %                        NOTA ist es aber noch nicht !!!!
# 
# ProjectedData = 'NOCH NICHT IMPEMENTIERT'



 }

