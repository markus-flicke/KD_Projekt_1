plotacor <-
function(Data, maxlag=length(Data)-1){

# function plotacor(data, maxlag); 
# % P = plotacor(data);
# %
# % in \dbt\Plot\
# %
# % DESCRIPTION
# % (Partial) Autocorrelation of a time series
# %
# % INPUT
# % data         data vector
# % maxlag       maximum lag
# % 
# % OUTPUT
# % none
# 
# % Version: FAB 4.7.04 (first version)
# 
# if nargin < 2
#     maxlag = size(data,1)-1;
# end    
# 
# figure;
# subplot(3,1,1);
# autocorr(data); ylabel(''); xlabel('');
# subplot(3,1,2);    
# autocorr(data,maxlag); ylabel(''); xlabel('');
# subplot(3,1,3);    
# parcorr(data); ylabel(''); xlabel('');

 par(mfcol=c(3,1))
 acf(Data,type="correlation")
 acf(Data,lag.max=maxlag)
 acf(Data,type="partial") 

 }

