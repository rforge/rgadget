survey.index <- function(sim){
  ##Calculates the total catch  
  opt <- sim$opt
  immIndex <- as.data.frame.table(sim$immNumRec,stringsAsFactors=FALSE)
  immIndex$year <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  immIndex$step <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  immIndex <- immIndex[immIndex$step==opt$survstep,]
  immSurveyAgg <- aggregate(immIndex$Freq,
                            by=list(
                              year=immIndex$year,
                              step=immIndex$step,
                              area=sprintf('area%s',immIndex$area),
                              age=ifelse(immIndex$age==1,'age1','ageother')),
                            sum)
  
  matIndex <- as.data.frame.table(sim$matNumRec,stringsAsFactors=FALSE)
  matIndex$year <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  matIndex$step <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  matIndex <- matIndex[matIndex$step==opt$survstep,]
  matSurveyAgg <- aggregate(matIndex$Freq,
                            by=list(
                              year=matIndex$year,
                              step=matIndex$step,
                              area=sprintf('area%s',matIndex$area),
                              age=rep('ageother',dim(matIndex)[1])),
                            sum)
  
  temp <- exp(rnorm(1,0,opt$survey.sigma^2)-opt$survey.sigma^2/2)
  SurveyIndex <- immSurveyAgg
  SurveyIndex$x[SurveyIndex$age=='ageother'] <-
    SurveyIndex$x[SurveyIndex$age=='ageother'] +
      matSurveyAgg$x
  SurveyIndex$x <- SurveyIndex$x*temp
  SurveyIndex$time <- SurveyIndex$year+(SurveyIndex$step - 1)/opt$numoftimesteps

  class(SurveyIndex) <- c('Rgadget',class(SurveyIndex))
  attr(SurveyIndex,'formula') <- x~time|area
  attr(SurveyIndex,'plotGroups') <- 'age'
  attr(SurveyIndex,'plotType') <- 'l'
  attr(SurveyIndex,'xaxis') <- 'Year'
  attr(SurveyIndex,'yaxis') <- 'Survey Index'
  
  return(SurveyIndex)
}

survey.indexlen <- function(sim){
  opt <- sim$opt
  immIndex <- as.data.frame.table(sim$immNumRec,stringsAsFactors=FALSE)
  immIndex$year <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  immIndex$step <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  immIndex <- immIndex[immIndex$step==opt$survstep,]
#  immIndex$length <- as.numeric(immIndex$length)
  immIndex$length.group <- cut(immIndex$length,
                               breaks=opt$length.groups,
                               labels=sprintf('lengp%s',
                                 1:(length(opt$length.groups)-1)))
  immSurveyLen <- aggregate(immIndex$Freq,
                            by=list(
                              year=immIndex$year,
                              step=immIndex$step,
                              area=sprintf('area%s',immIndex$area),
                              length.group=immIndex$length.group),
                            sum)
  
  matIndex <- as.data.frame.table(sim$matNumRec,stringsAsFactors=FALSE)
  matIndex$year <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  matIndex$step <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  matIndex <- matIndex[matIndex$step==opt$survstep,]
  matIndex$length.group <- cut(as.numeric(matIndex$length),
                               breaks=opt$length.groups,
                               labels=sprintf('lengp%s',
                                 1:(length(opt$length.groups)-1)))
  matSurveyLen <- aggregate(matIndex$Freq,
                            by=list(
                              year=matIndex$year,
                              step=matIndex$step,
                              area=sprintf('area%s',matIndex$area),
                              length.group=matIndex$length.group),
                            sum)
  
  SurveyIndex <- immSurveyLen
  SurveyIndex$x <- (SurveyIndex$x + matSurveyLen$x)*
    exp(rnorm(length(SurveyIndex$x),0,opt$survey.sigma^2)-
        opt$survey.sigma^2/2)
  
  SurveyIndex$time <- SurveyIndex$year+(SurveyIndex$step - 1)/opt$numoftimesteps
  
  class(SurveyIndex) <- c('Rgadget',class(SurveyIndex))
  attr(SurveyIndex,'formula') <- x~time|area
  attr(SurveyIndex,'plotGroups') <- 'length.group'
  attr(SurveyIndex,'plotType') <- 'l'
  attr(SurveyIndex,'xaxis') <- 'Year'
  attr(SurveyIndex,'yaxis') <- 'Survey Length Index'

  return(SurveyIndex)
}

catch.ldist <- function(sim){
  opt <- sim$opt
  immIndex <- as.data.frame.table(sim$immCcomm,stringsAsFactors=FALSE)
  immIndex$year <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  immIndex$step <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  #immIndex <- immIndex[immIndex$step==opt$survstep,]
  immIndex$length <- as.numeric(immIndex$length)
  immIndex$length.group <- cut(immIndex$length,
                               breaks=opt$length.groups,
                               labels=sprintf('lengp%s',
                                 1:(length(opt$length.groups)-1)))
  immCatchLen <- aggregate(immIndex$Freq,
                           by=list(
                             year=immIndex$year,
                             step=immIndex$step,
                             area=sprintf('area%s',immIndex$area),
                             age=rep('allages',length(immIndex$year)),
                             length=as.numeric(immIndex$length)),
                           sum)

  matIndex <- as.data.frame.table(sim$matCcomm,stringsAsFactors=FALSE)
  matIndex$year <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  matIndex$step <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  
  matCatchLen <- aggregate(matIndex$Freq,
                           by=list(
                             year=matIndex$year,
                             step=matIndex$step,
                             area=sprintf('area%s',matIndex$area),
                             age=rep('allages',length(matIndex$year)),
                             length=as.numeric(matIndex$length)),
                           sum)

  SurveyIndex <- immCatchLen
  SurveyIndex$x <- (SurveyIndex$x + matCatchLen$x)*
    exp(rnorm(length(SurveyIndex$x),0,opt$survey.sigma^2)-
        opt$survey.sigma^2/2)
  SurveyIndex$length <- as.numeric(SurveyIndex$length)
  SurveyIndex$time <- SurveyIndex$year+(SurveyIndex$step - 1)/opt$numoftimesteps

  class(SurveyIndex) <- c('Rgadget',class(SurveyIndex))
  attr(SurveyIndex,'formula') <- x~time|area
  attr(SurveyIndex,'plotGroups') <- 'length.group'
  attr(SurveyIndex,'plotType') <- 'l'
  attr(SurveyIndex,'xaxis') <- 'Year'
  attr(SurveyIndex,'yaxis') <- 'Catch Length Index'
  
  
  return(SurveyIndex)
}

survey.ldist <- function(sim){
  opt <- sim$opt
  immIndex <- as.data.frame.table(sim$immCsurv,stringsAsFactors=FALSE)
  immIndex$year <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  immIndex$step <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  immIndex <- immIndex[immIndex$step==opt$survstep,]
  
  immIndex$length.group <- cut(as.numeric(immIndex$length),
                               breaks=opt$length.groups,
                               labels=sprintf('lengp%s',
                                 1:(length(opt$length.groups)-1)))
  immSurveyLen <- aggregate(immIndex$Freq,
                           by=list(
                             year=immIndex$year,
                             step=immIndex$step,
                             area=sprintf('area%s',immIndex$area),
                             age=rep('allages',length(immIndex$year)),
                             length=as.numeric(immIndex$length)),
                           sum)

  matIndex <- as.data.frame.table(sim$matCsurv,stringsAsFactors=FALSE)
  matIndex$year <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  matIndex$step <- sapply(strsplit(matIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  matIndex <- matIndex[matIndex$step==opt$survstep,]
  matSurveyLen <- aggregate(matIndex$Freq,
                           by=list(
                             year=matIndex$year,
                             step=matIndex$step,
                             area=sprintf('area%s',matIndex$area),
                             age=rep('allages',length(matIndex$year)),
                             length=as.numeric(matIndex$length)),
                           sum)

  SurveyIndex <- immSurveyLen
  SurveyIndex$x <- (SurveyIndex$x + matSurveyLen$x)*
    exp(rnorm(length(SurveyIndex$x),0,opt$survey.sigma^2)-
        opt$survey.sigma^2/2)
  SurveyIndex$time <- SurveyIndex$year+(SurveyIndex$step - 1)/opt$numoftimesteps
#  SurveyIndex$length <- as.numeric(SurveyIndex$length)
  class(SurveyIndex) <- c('Rgadget',class(SurveyIndex))
  attr(SurveyIndex,'formula') <- x~time|area
  attr(SurveyIndex,'plotGroups') <- 'length.group'
  attr(SurveyIndex,'plotType') <- 'l'
  attr(SurveyIndex,'xaxis') <- 'Year'
  attr(SurveyIndex,'yaxis') <- 'Survey Length Index'
  return(SurveyIndex)
}

age.length.key <- function(sim,age.agg,len.agg){
   ## age length table
  opt <- sim$opt
  alk.table <- function(catch,age.agg,len.agg){
    catch.table <- as.data.frame.table(catch,stringsAsFactors=FALSE)
    catch.table$year <- sapply(strsplit(catch.table$time,'_'),
                               function(x) as.numeric(x[2]))
    catch.table$step <- sapply(strsplit(catch.table$time,'_'),
                               function(x) as.numeric(x[4]))
    catch.table$age.agg <-
      1 + round((as.numeric(catch.table$age) - opt$minage)/age.agg)
    catch.table$length.agg <-
      1 + round((as.numeric(catch.table$length) - opt$minlen)/len.agg)
    tmp <- aggregate(catch.table$Freq,
                     by=list(
                       year=catch.table$year,
                       step=catch.table$step,
                       area=paste('area',catch.table$area,sep=''),
                       age=paste('age',catch.table$age.agg,sep=''),
                       length=paste('len',catch.table$length.agg,sep='')),
                     sum)    
    if(len.agg==(opt$maxlen-opt$minlen))
      tmp$length <- 'alllen'
    if(age.agg==(opt$maxage))
      tmp$age <- 'allages'
    return(tmp)
  }

  immComm <- alk.table(sim$immCcomm,
                       #opt,
                       age.agg,
                       len.agg)
  matComm <- alk.table(sim$matCcomm,
                       #opt,
                       age.agg,
                       len.agg)
  immSurv <- alk.table(sim$immCsurv,
                       #opt,
                       age.agg,
                       len.agg)
  matSurv <- alk.table(sim$matCsurv,
                       #opt,
                       age.agg,
                       len.agg)
  comm <- merge(immComm,matComm,
                by=c('year','step','area','length','age'),
                all=TRUE,
                suffixes=c('imm','mat'))
  comm$ximm[is.na(comm$ximm)] <- 0
  comm$xmat[is.na(comm$xmat)] <- 0
  comm$total.catch <- comm$ximm + comm$xmat
  comm <- comm[!(comm$total.catch==0),]
  comm$ximm <- NULL
  comm$xmat <- NULL
  comm$fleet <- 'comm'
  surv <- merge(immSurv,matSurv,
                by=c('year','step','area','length','age'),
                all=TRUE,
                suffixes=c('imm','mat'))
  surv$ximm[is.na(surv$ximm)] <- 0
  surv$xmat[is.na(surv$xmat)] <- 0
  surv$total.catch <- surv$ximm + surv$xmat
  surv <- surv[!(surv$total.catch==0),]
  surv$ximm <- NULL
  surv$xmat <- NULL
  surv$fleet <- 'surv'
  alk <- rbind(surv,comm)
  return(alk)
}

catch.in.kilos <- function(sim){
  opt <- sim$opt
  commAmount <- apply(apply(sim$immCcomm,c(1,3,4),
                            function(x) opt$w*x),
                      c(2,4),sum) +
                        apply(apply(sim$matCcomm,c(1,3,4),
                                    function(x) opt$w*x),
                              c(2,4),sum)
  commAmount <- as.data.frame.table(commAmount,stringsAsFactors=FALSE)
  commAmount$year <- sapply(strsplit(commAmount$time,'_'),
                          function(x) as.numeric(x[2]))
  commAmount$step <- sapply(strsplit(commAmount$time,'_'),
                          function(x) as.numeric(x[4]))
  commAmount$time <- NULL
  commAmount <- commAmount[commAmount$Freq!=0,]
  commAmount$area <- sprintf('area%s',commAmount$area)
  commAmount$fleet <- 'comm'
  commAmount <- commAmount[c('year','step','area','fleet','Freq')]
  return(commAmount)
}

plot.Rgadget <- function(dat){
  dat$plotGroups <- dat[[attr(dat,'plotGroups')]]
  xyplot(attr(dat,'formula'),
         data=dat,
         groups=plotGroups,
         type=attr(dat,'plotType'),
         plot.points=FALSE,
         auto.key = list(),
         ylab=attr(dat,'yaxis'),
         xlab=attr(dat,'xaxis'),
         ref=TRUE) 
}
