requireRpackage=function(name,biocite=FALSE,Verbose=FALSE){
#  requireRpackage(name)
# Loads or automatically installs and loads a R package.
# If package cant be installed, the function stops
#
# Input
# name      string with name of package
#
# Optional
# biocite   =FALSE, if TRUE Loading Package from biocite instead of CRAN
# Verbose  =TRUE: all 'simple' diagnostic messages on package loading
# Output
# 
# author: MT 09/2014
# 1.Editor: MT 06/2015: umbenannt, argumente nach ALUs Definitionen angepasst
  if(!Verbose){
    
      if(suppressMessages(require(name,character.only = TRUE))){
          #print(paste0(name," is loaded correctly"))
      }else{
        if(biocite){
          suppressMessages(source("http://bioconductor.org/biocLite.R"))
          suppressMessages(biocLite(name))
          requireRpackage(name,biocite=biocite,Verbose=Verbose)
        }else{
         # print(paste0("trying to install ",name))
          install.packages(name)
            if(require(name,character.only = TRUE)){
                #print(paste0(name," installed and loaded"))
            }else{
                stop(paste0("could not install ",name))
            }
  		    requireRpackage(name,suppress)
        }# if biocite
      }       
    
    
  }else{

      if(require(name,character.only = TRUE)){
          print(paste0(name," is loaded correctly"))
      }else{
        if(biocite){
          source("http://bioconductor.org/biocLite.R")
          biocLite(name)
          requireRpackage(name,suppress)
        }else{
          print(paste0("trying to install ",name))
          install.packages(name)
            if(require(name,character.only = TRUE)){
                print(paste0(name," installed and loaded"))
            }else{
                stop(paste0("could not install ",name))
            }
			    requireRpackage(name,biocite=biocite,Verbose=Verbose)
        }# if biocite
      }   # if requite

  } #if !Verbose

  invisible()
}# end function