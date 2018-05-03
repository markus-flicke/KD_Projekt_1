ClearWorkspace <- function(){
#  ClearWorkspace()
# removes all Objects from Workspace
# usufull to delete older versions of functions etc

  rm(list = ls());   # will remove everything shown by ls().
  #  by default objects that start with a "." are omitted from the results of ls(). 

 }      #end ClearWorkspace