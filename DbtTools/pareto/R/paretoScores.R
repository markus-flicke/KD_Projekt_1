`paretoScores` <-
function(data,cls){

# function p = ParetoScores(data,cls,fast)
# % p = ParetoScores(data,cls,fast)
# %
# % Calulates Pareto score (see ParetoScore.m) for
# % all features and all classes
# %
# % INPUT
# % data      data matrix, features in columns
# % cls       class vector
# %
# % OPTIONAL
# % fast      fast version without pareto radius
# %
# % OUTPUT
# % p         Pareto scores
# % b         Bayes errors
# 
# c = unique(cls);
# p = zeros(size(data,2),length(c));
# for i = 1:size(p,1)
#     for j = 1:size(p,2)
#         class = find(cls==c(j));
#         rest  = find(cls~=c(j));
#         p(i,j) = ParetoScore(data(class,i),data(rest,i),1);
#     end
# end

	c <- unique(cls)
	p <- matrix(0,nrow=nrow(data),ncol=length(c))
 	for(i in 1:nrow(p)){
 		for(j in 1:ncol(p)){
 			class <- cls==c[j]
 			rest <- cls!=c[j]
 			print(class)
 			val <- paretoScore(data[class,i],data[rest,i],TRUE)
 			p[i,j] <- val$p	
 		}
 		
 		
 	}

 }

