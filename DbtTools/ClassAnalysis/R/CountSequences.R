CountSequences <- function (cls) {
cls <- as.matrix(cls)
diff2next <- c(1, diff(cls) != 0)
sequenceNumbers <- cumsum(diff2next)
hlp <- cbind(cls, diff2next, sequenceNumbers)


return( list(sequenceNumbers = sequenceNumbers, hlp = hlp))
}
# function SequenceNumbers = CountSequences(Cls);
# % [UniqueClasses,CountPerClass,NrOfClasses,ClassPercentages] = ClassCount(cls);
# % assign an increasing  number to consequtive sequencences of the same class label
# % INPUT
# % Cls(1:n)
# % OUTPUT
# % UniqueClasses(NrOfClasses)      the  NrOfClasses unique classes in cls
# % CountPerClass(NrOfClasses,n)    CountPerClass(i) is the Count of the data points in UniqueClasses(i)
# % NrOfClasses                     the number of classes
# 
# Cls = colvector(Cls);
# Diff2Next = [1;diff(Cls)~=0]; % enthalt eine 1 wenn sich eine neue sequenz anbahnt, sonst 0
# SequenceNumbers = cumsum(Diff2Next);
# hlp=[Cls Diff2Next SequenceNumbers]

