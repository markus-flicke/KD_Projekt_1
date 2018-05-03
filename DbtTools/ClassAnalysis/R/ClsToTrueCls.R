ClsToTrueCls <- function(trueCls, givenCls) {
  #ctable <- ContingencyTable(trueCls, givenCls)
  normTrueCls <- NormalizeCls(trueCls)[[1]]
  normGivenCls <- NormalizeCls(givenCls)[[1]]
  
  V <- RenameForCoincidence(givenCls=normGivenCls, standardCls=normTrueCls)
  
  return(list(
    accuracy = V$bestAccuracy,
    renamedCls = V$renamedCls
  ))
} 