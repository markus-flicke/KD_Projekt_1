ClassEntropy <- function(cls, LogBasis) {
    #  calulate the Entropy =-sum(p log(p)) (Information) for the cls -p log(p)
    # INPUT
    # cls(d)            cls(i) == ClusterNumber of data(i,:)
    # OPTIONAL
    # LogBasis          the basis for the logatithm, default ==2
    # OUTPUT
    # Entropy             sum of EntropyPerClass
    # EntropyPerClass     partial entropy per uniue class
    # UniqueClasses       ordered classes 
    # P                   vector of partial probabilities corresponding to UniqueClasses 
    # NormalizedEntropy = Entropy/UniformEntropy; In order to compare Entropies of different class numbers 
    # UniformEntopy       the Entropy of a Classification with the same number of classes as in cls, but 
    #                     the same number of elements in each class

    if(missing(LogBasis))
        LogBasis = 2
    uniqueClasses <- ClassCount(cls)[[1]]
    numberInClass <- ClassCount(cls)[[2]]
    numberOfClasses <- length(uniqueClasses)
    totalNumber <- length(cls)
    entropyPerClass <- rep(0, numberOfClasses)

    if (( totalNumber < 2) || (numberOfClasses < 2)) {
        entropy <- 0
        p <- 1
        uniformEntropy <- 1
        normalizedEntropy <- 0
    }
    else {
        p <- numberInClass / totalNumber
        entropyPerClass <- - p * log(p,LogBasis)
        entropy <- sum(entropyPerClass)
        pUniform <- 1 / numberOfClasses
        uniformEntropy <- (- pUniform * log(pUniform,LogBasis)) * numberOfClasses
        normalizedEntropy <- entropy / uniformEntropy
    }


    return(list(Entropy = entropy, EntropyPerClass = entropyPerClass, UniqueClasses = uniqueClasses, P = p, NormalizedEntropy = normalizedEntropy, UniformEntropy = uniformEntropy ))
}
