clearAll <- function(){
  exceptions = c("checkDBT", "CurrentDirectory", "detachEverything", "extractPackageInfo",
                 "FCPS", "forceUnloadPackage", "GOdataDi", "installCranPackages",
                 "installDBTDependencies", "missingDBTPackages", "nrOfPackages",
                 "printAndLog", "ReDi", "removeDBT", "setSVNdir", "sourceDBTPackage",
                 "SubversionDirectory", "SVNLibraryFile", "SVNsource", "updateDBT"
  )



  rm(list=  setdiff(ls(), exceptions) )
}
