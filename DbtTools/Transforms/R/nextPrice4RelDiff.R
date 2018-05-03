`nextPrice4RelDiff` <- function(priceYesterday, relDiff){

# function PreisHeute = NextPreis4RelDiff(PreisGestern,RelDiff);
# % Heutigen Preis aus gestrigem Preis und RelDiff errechnen
# % INPUT
# % PreisGestern(1:AnzKurse)    gestrige Preise tageweise in den Spalten. 
# % RelDiff(1:AnzKurse)         Relative Differenzen, tageweise in den Spalten. 
# % OUTPUT
# % PreisHeute(1:AnzKurse)      heutige Preise tageweise in den Spalten. 
# 
# % ALU Dez. 2007
# % Pheute= Pgestern (2+RelDiff) / (2-RelDiff)
# 
# 
# PreisHeute = PreisGestern .* ((2+RelDiff) ./ (2-RelDiff));

priceOfToday = priceYesterday * ((2+relDiff) / (2-relDiff))

return(priceOfToday)


 }

