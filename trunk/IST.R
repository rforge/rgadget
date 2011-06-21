source('function.R')
source('whaleStock.R')
source('summaryFunc.R')
library(plyr)
library(reshape)
library(aod)
library(multicore)
opt <- gadget.options()
opt$stocks <- c('W','C1','C2','C3','E','S')

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

opt$num.tags <- 100
names(opt$num.tags) <- 'EG'

opt$tag.loss <- 1
opt$recapture.lambda <- 2
opt$gender.division <- c(Male=1/2,Female=1/2)

## birth parameters obtained in the IST-trial 
opt$density.z <- 2.38980
opt$age.of.parturation <- 6
opt$resiliance.a <- c(r1=0.17031,r2=0.42577,r4=0.68123)
opt$avg.B <- 2/(opt$maxage-opt$age.of.parturation)
opt$msyl <- 0.72
opt$msyr <- c(r1=0.01,r2=0.02,r4=0.04)

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
opt.h3$init.abund <- c(7595, 5260, 3362,  8183, 6613,  7841)
opt.h4$init.abund <- c(7266,  3317,  5422,  7730,  7064,  7995)
names(opt.h3$init.abund) <- opt$stocks
names(opt.h4$init.abund) <- opt$stocks




power.analysis <- function(rec){
#  tmp.func <- function(x){
#    tmp.year <- negbin(value~year,~1,x,method='SANN')
#    tmp.0 <- negbin(value~1,~1,x,method='SANN')
#    return(anova(tmp.year,tmp.0)@anova.table$P[2])
#  }

#  AIC.year <- ddply(rec,'variable', tmp.year)
#  AIC.0 <- ddply(rec,'variable', tmp.0)

  tagLik <- function(y,theta){
    
    sum(-(lgamma(y+theta)+theta*log(1/3) + y*log(2/3) - (lgamma(y+1) + lgamma(theta))))
  }
  
  tagMin <- function(x,y){
    if(x[1] < 0 | x[2] < 0)
      1e6
    else
      tagLik(y,2:10*x[1]+x[2])
  }
  
  tagMin0 <- function(x,y){
    if(x < 0)
      1e6
    else
      tagLik(y,x)
  }

  tmp.func <- function(x){
    tmp.year <- optim(c(1,1),tagMin,y=x$value,method='SANN')
    tmp.0 <- optim(1,tagMin0,y=x$value,method='SANN')
    lrt <- 2*(tmp.0$value - tmp.year$value)
    return(1-pchisq(lrt,df=1))
  }
  
  return(ddply(rec,'variable', tmp.func))
  
}
if(FALSE){
  p.h3 <- power.analysis(rec.h3)
  p.h4 <- power.analysis(rec.h4)
  
  print(sprintf('Hypothesis 4 rejected when true: %s',sum(p.h4<1)))
  print(sprintf('Hypothesis 3 rejected when true: %s',sum(p.h3>1)))
}



run.tag.experiment <- function(num.tags){
  opt.h4$num.tags <- num.tags
  names(opt.h4$num.tags) <- 'EG'
  opt.h3$num.tags <- num.tags
  names(opt.h3$num.tags) <- 'EG'
  ## run the simulation
  sim.h4 <- Rgadget(opt.h4)
  sim.h3 <- Rgadget(opt.h3)
  
  ## tags..
  rec.h4 <- tagging.recaptures(sim.h4,2,1000)
  rec.h4$rec$year <- 2:10
  rec.h3 <- tagging.recaptures(sim.h3,2,1000)
  rec.h3$rec$year <- 2:10
  ## test power of a regular tagging experiment
  p.h3 <- power.analysis(rec.h3$rec)
  p.h4 <- power.analysis(rec.h4$rec)
 
  save(sim.h4,sim.h3,rec.h4,rec.h3,p.h3,p.h4,
       file=sprintf('tag%s.RData',num.tags))
  return(list(h3=p.h3,h4=p.h4))
}

hypo.test <- mclapply(100*(1:15),run.tag.experiment)

rho.test <- within(list(h3=1:15,h4=1:15),
                   for(i in 1:15){
                     load(sprintf('tag%s.RData',i*100))
                     qq <- quantile(rec.h4$rho,0.95)
                     h4[i] <- sum(rec.h4$rho>qq)/1000
                     h3[i] <- sum(rec.h3$rho>qq)/1000
                   }
                   )
