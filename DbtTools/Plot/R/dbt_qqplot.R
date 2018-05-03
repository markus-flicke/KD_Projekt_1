dbt_qqplot <-
function(x, y, pstyle=1){

# function [px,py]= qqplot(x,y,ps);
# % [px,py]= qqplot(x,y,ps);
# % Plot empirical quantile vs empirical quantile 
# % INPUT
# % x,y the data points
# % OPTIONAL
# % ps          symbol uset to plot
# % filename    if given plot to file
# % OUTPUT
# % px = percentiles(x);
# % py = percentiles(y);
# 
# % ALU 2003
# 
# if nargin<3, ps = '.'; end
# 
# px = percentiles(x);
# py = percentiles(y);
# plot(px,py,ps);
# xlabel('Data X');
# ylabel('Data Y');
# title('Q/Q plot of two variables');


qqplot(x, y, xlab="Data X", ylab="Data Y", pch=pstyle, col="blue", main="Q/Q plot of two variables")

}

