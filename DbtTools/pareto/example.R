InDirectory   =paste0(SubversionDirectory(),'PUB/dbt/dbt.pareto/data')
LrnFileName     = 'Example.lrn'
allinfo= ReadLRN(LrnFileName,InDirectory)
Data=allinfo$Data
PDEplot(Data)