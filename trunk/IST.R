source('function.R')
opt <- gadget.options()
opt$stocks <- c('W','C1','C2','C3','E','S')
opt$init.abund <- c(3835,  3173,  2681,  5862,  3965,  6225)
names(opt$init.abund) <- opt$stocks

## time and space
opt$areas <- c('CE','WG','EG','WI','EIF','N','SP')
opt$numofareas <- length(opt$areas)

opt$numoftimesteps <- 1
opt$dt <- 1

## length and age
opt$maxage <- 25
opt$numofagegroups <- opt$maxage + 1

opt$minlen <- 1
opt$maxlen <- 2
opt$numoflgroups <- 1
opt$z <- 0.08

## migration and dispersion
opt$gamma.mix <- 0.8
opt$alpha.mix <- 0.05
opt$mixing <- matrix(0,ncol=6,nrow=7,dimnames=list(areas=opt$areas,stocks=opt$stocks))
opt$mixing['CE','W'] <- opt$gamma.mix
opt$mixing['WG','W'] <- 1-opt$gamma.mix
opt$mixing[,'C1'] <- c(opt$alpha.mix*opt$gamma.mix,
                       opt$alpha.mix*(1-opt$gamma.mix),
                       1-2*opt$alpha.mix,
                       opt$alpha.mix,0,0,0)
opt$mixing[c('EG','WI','EIF'),'C2'] <- c(opt$alpha.mix,
                                         1-2*opt$alpha.mix,
                                         opt$alpha.mix)
opt$mixing[c('WI','EIF','N'),'C3'] <- c(opt$alpha.mix,
                                         1-2*opt$alpha.mix,
                                         opt$alpha.mix)
opt$mixing['N','E'] <- 1
opt$mixing['SP','S'] <- 1

## dispersion
opt$dispersion <- matrix(0,nrow=length(opt$stocks),ncol=length(opt$stocks),
                         dimnames = list(To = opt$stocks,
                           From = opt$stocks))
diag(opt$dispersion) <- 1
opt$dispersion['C1',sprintf('C%s',1:3)] <- c(0.95,0.05,0)
opt$dispersion['C2',sprintf('C%s',1:3)] <- c(0.05572320,1-0.35572320,0.3)
opt$dispersion['C3',sprintf('C%s',1:3)] <- c(0,0.00157109,1-0.00157109)

## harvest
opt$fleets <- 'Comm'
opt$doescatchcomm <- 'WI'
opt$doescathcsurv <- ''
opt$quota <- 150
names(opt$quota) <- 'WI'
opt$salphacomm <- -3.6/0.57
opt$sbetacomm <- 1/0.57

opt$doeseat <- 0
opt$doesgrow <- 0

opt$mort <- rep(opt$z,maxage+1)
opt$doescatchsurv <- NULL

opt$num.tags <- 100
names(opt$num.tags) <- 'EG'

opt$tag.loss <- 1
