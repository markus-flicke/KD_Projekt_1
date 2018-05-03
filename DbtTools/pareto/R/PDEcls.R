

PDEcls=function(data, cls, show_legend=FALSE){
#require('RPMG')

# function PDEcls(data, cls, show_legend, bw);
# % PDEcls(data, cls, show_legend, bw);
# %
# % in /dbt/Pareto/
# %
# % DESCRIPTION
# % plot PDE densities for each class
# %
# % INPUT
# % data               data to be plotted
# % cls                class number for each dataset
# % show_legend        flag whether to show legend. otional, default = 0
# % bw                 flag whether to plot black and white, default = 0
# %
# % OUTPUT
# % none
# 
# % Version: FAB 4.7.04 (first version)
# 
   

	data <- as.matrix(data)

	classes <- sort(na.last=T,unique(cls))
	paretoRadius <- ParetoRadius(data)
	requireNamespace('RPMG')
	colors <- RPMG::rainbow.colors(length(classes))
	
	
	for(j in 1:length(classes)){
		rows <- cls == classes[j]
		if(j==1){
			PDEplot(data[rows,],paretoRadius,col=colors[j])
			}
		else
			PDEplot(data[rows,],paretoRadius,col=colors[j],points=TRUE)

	}

	if(show_legend)
	{
		legend('topright',paste(classes),fill=colors)
	}
		
	 


 }

