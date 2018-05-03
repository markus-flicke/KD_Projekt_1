qqlognorm <- function(x, VarName = "Data", pstyle = 1, MU, SIGMA){
  # qqlognorm(x,Name,LineType,MU,SIGMA);
  # Quantile/Quantile = QQ-Plot im Vergleich. zur LogNormalverteilung
  # INPUT
  # x(1:n)            data may contain NaN
  #
  # OPTIONAL
  # pstyle           bezeichnung der y-achse
  # MU,SIGMA          parameters of LogNormal distribution
  #                   estimated if not given
  # 
  # OUTPUT
  # qqx,qqy   die Punkte des qq-plots 
  #  
  # AUTHOR
  # ALU 2017 (Matlab)
  # FP 2017 (R Port)
  
  
  if (missing(MU)) {
    if (nanmedian(x) > 0) {
      MU = log(nanmedian(x))
    } else {
      stop("No MU given and median of input data is 0 or less, so no estimation possible")
    }
  }
  if (missing(SIGMA)) {
    if (mean(x, na.rm = T) > 0) {
      SIGMA = sqrt(2 * (log(mean(x, na.rm = T)) - MU))
    } else {
      stop("No SIGMA given and mean of input data is 0 or less, so no estimation possible")
    }
  }
  n = length(x)
  RandLogNorm = rlnorm(n, MU, SIGMA)
  
  qqplot(
    x = x,
    y = RandLogNorm,
    xlab = cat("LogNorm(", MU, " , ", SIGMA, " )", sep = ""),
    ylab = VarName,
    main = "QQplot for LogNormal distribution",
    pch = pstyle
  )
  return(invisible(list(
    qqx = x,
    qqy = RandLogNorm,
    MU = MU,
    SIGMA = SIGMA
  )))
  
}