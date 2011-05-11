source('function.R')
source('whaleStock.R')
source('summaryFunc.R')
library(plyr)
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

## harvest
opt$fleets <- 'Comm'
opt$doescatchcomm <- 'WI'
opt$doescathcsurv <- ''
opt$quota <- 150
names(opt$quota) <- 'WI'
opt$salpha <- c(Male=-3.6/0.57,Female=-4.1)
opt$sbeta <- c(Male=1/0.57,Female=1)

opt$doeseat <- 0
opt$doesgrow <- 0

opt$mort <- rep(opt$z,opt$maxage+1)
opt$doescatchsurv <- NULL

opt$num.tags <- 500
names(opt$num.tags) <- 'EG'

opt$tag.loss <- 1
opt$recapture.lambda <- 2
opt$gender.division <- c(Male=1/2,Female=1/2)


## migration and dispersion
opt$gamma.mix <- 0.8

opt.h4 <- opt
opt.h3 <- opt
opt.h4$alpha.mix <- 0.05
opt.h3$alpha.mix <- 0
mixing.matrix <- function(opt){
  opt$mixing <- matrix(0,ncol=6,nrow=7,
                       dimnames=list(area=opt$areas,stock=opt$stocks))
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
  return(opt)
}

opt.h4 <- mixing.matrix(opt.h4)
opt.h3 <- mixing.matrix(opt.h3)
opt.h3$mixing[c('CE','WG','EG'),'C1'] <- c(0.1*opt$gamma.mix,
                                           0.1*(1-opt$gamma.mix),
                                           0.9)

## dispersion
dispersion.matrix <- function(opt){
  opt$dispersion <- matrix(0,nrow=length(opt$stocks),
                           ncol=length(opt$stocks),
                           dimnames = list(To = opt$stocks,
                             From = opt$stocks))
  diag(opt$dispersion) <- 1
  opt$dispersion['C1',sprintf('C%s',1:3)] <- c(0.95,0.05,0)
  opt$dispersion['C2',sprintf('C%s',1:3)] <- c(0.05572320,1-0.35572320,0.3)
  opt$dispersion['C3',sprintf('C%s',1:3)] <- c(0,0.00157109,1-0.00157109)
  return(opt)
}

opt.h3 <- dispersion.matrix(opt.h3)


## run the simulation
sim.h4 <- Rgadget(opt.h4)
sim.h3 <- Rgadget(opt.h3)

## tags..
rec.h4 <- tagging.recaptures(sim.h4,2,100)
rec.h3 <- tagging.recaptures(sim.h3,2,100)
