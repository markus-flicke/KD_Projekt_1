dbt_screeplot <- function(Data, Legend = TRUE) {
  num <- 1:length(Data)
  ySingle <- Data / sum(Data)
  yCumulative <- cumsum(Data) / sum(Data)
  # plot single percentage of occurance
  plot(
    num,
    ySingle,
    col = "blue",
    type = "p",
    pch = 1,
    ylim = c(min(0, min(
      ySingle, yCumulative
    )), max(1, max(
      ySingle, yCumulative
    ))),
    xlab = "",
    ylab = ""
  ) # points
  lines(num, ySingle, col = "blue") # line
  # plot cumulative perccentage of occurance
  points(num, yCumulative, col = "red", pch = 4) # points
  lines(num, yCumulative, col = "red") # line
  grid()
  if (Legend) {
    legend(
      x = 1,
      y = max(1, max(max(ySingle), max(yCumulative))),
      legend = c("single percentage", "cumulative percentage"),
      col = c("blue", "red"),
      text.col = "black",
      lty = c(1, 1),
      pch = c(1, 4),
      bg = "gray90"
    )
  }
  
  
}
