figure <-
function ()
# works like MATHLAB's fuktion
{   
  sysname = Sys.info()['sysname']
  if(sysname == 'Windows'){
    grDevices::windows()
  }else if(sysname == 'Linux'){
    grDevices::X11()
  }else{
    grDevices::quartz()
  }
}  #END FUNCTION

