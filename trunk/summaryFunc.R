##' Calculates the survey indices for the simulated stocks. 
##' @title Survey indices
##' @param sim Results from a Rgadget simulation
##' @return Dataframe with the survey indices 
##' @author Bjarki Þór Elvarsson
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
  attr(SurveyIndex,'plotFun') <- 'xyplot'  
  return(SurveyIndex)
}
##' Calculate the survey length index based on the provided lengthgroupsw
##' @title Survey length index
##' @param sim Results from a Rgadget simulation
##' @return Dataframe containing the length index from the 
##' @author Bjarki Þór Elvarsson
survey.indexlen <- function(sim){
  opt <- sim$opt
  immIndex <- as.data.frame.table(sim$immNumRec,stringsAsFactors=FALSE)
  immIndex$year <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[2]))
  immIndex$step <- sapply(strsplit(immIndex$time,'_'),
                          function(x) as.numeric(x[4]))
  immIndex <- immIndex[immIndex$step==opt$survstep,]
  immIndex$length <- as.numeric(immIndex$length)
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
  matIndex$length <- as.numeric(matIndex$length)
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
  attr(SurveyIndex,'plotFun') <- 'xyplot'
  return(SurveyIndex)
}
##' Calculate the length distribution from catch (commercial fleet) by length groups and time
##' @title Length distribution from catch 
##' @param sim Results from a Rgadget simulation
##' @return Dataframe containing the length distribution
##' @author Bjarki Þór Elvarsson
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
  attr(SurveyIndex,'formula') <- x~length|year+area
  attr(SurveyIndex,'plotGroups') <- 'step'
  attr(SurveyIndex,'plotType') <- 'l'
  attr(SurveyIndex,'xaxis') <- 'Year'
  attr(SurveyIndex,'yaxis') <- 'Catch Length Index'
  attr(SurveyIndex,'plotFun') <- 'xyplot'  
  
  return(SurveyIndex)
}
##' Calculate the length distribution from the survey by length groups and time.
##' @title Survey length distribution 
##' @param sim Results from a Rgadget simulation
##' @return Dataframe containing the survey length distribution.
##' @author Bjarki Þór Elvarsson
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
  attr(SurveyIndex,'formula') <- x~length|year+area
  attr(SurveyIndex,'plotGroups') <- 'step'
  attr(SurveyIndex,'plotType') <- 'l'
  attr(SurveyIndex,'xaxis') <- 'Year'
  attr(SurveyIndex,'yaxis') <- 'Survey Length Index'
  attr(SurveyIndex,'plotFun') <- 'xyplot'
  return(SurveyIndex)
}
##' Calculates the age-length-key for the survey and commercial fleet.
##' @title Age length key
##' @param sim Results from a Rgadget simulation
##' @param age.agg The desired age aggregation
##' @param len.agg The desired length aggregation
##' @return Dataframe containing the age - length key
##' @author Bjarki Þór Elvarsson
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
      ordered(1 + round((as.numeric(catch.table$age) - opt$minage)/age.agg))
    levels(catch.table$age.agg) <- paste('age',
                                         levels(catch.table$age.agg),
                                         sep='')
    catch.table$length.agg <-
      ordered(1 + round((as.numeric(catch.table$length) - opt$minlen)/len.agg))
    levels(catch.table$length.agg) <- paste('len',
                                            levels(catch.table$length.agg),
                                            sep='')
    tmp <- aggregate(catch.table$Freq,
                     by=list(
                       year=catch.table$year,
                       step=catch.table$step,
                       area=paste('area',catch.table$area,sep=''),
                       age=catch.table$age.agg,
                       length=catch.table$length.agg),
                     sum)    
    if(len.agg==(opt$maxlen-opt$minlen))
      tmp$length <- ordered('alllen')
    if(age.agg==(opt$maxage))
      tmp$age <- ordered('allages')
    return(tmp)
  }

  immComm <- alk.table(sim$immCcomm,
                       age.agg,
                       len.agg)
  matComm <- alk.table(sim$matCcomm,
                       age.agg,
                       len.agg)
  immSurv <- alk.table(sim$immCsurv,
                       age.agg,
                       len.agg)
  matSurv <- alk.table(sim$matCsurv,
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

  alk$time <- alk$year + (alk$step-1)/4
  class(alk) <- c('Rgadget',class(alk))
  attr(alk,'formula') <- total.catch~as.numeric(age)+as.numeric(length)|area+fleet+time
  attr(alk,'plotGroups') <- ''
  attr(alk,'plotType') <- ''
  attr(alk,'xaxis') <- 'Year'
  attr(alk,'yaxis') <- 'Age - Length - Key'
  attr(alk,'plotFun') <- 'contour'
  return(alk)

}
##' Calculates the overall weigth of the catches by time step and area.
##' @title Catch in Kilos 
##' @param sim Results from a Rgadget simulation
##' @return Dataframe with the catch in kilos by timestep and ared.
##' @author Bjarki Þór Elvarsson
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
  commAmount$time <- commAmount$year+(commAmount$step - 1)/opt$numoftimesteps
  commAmount <- commAmount[commAmount$Freq!=0,]
  commAmount$area <- sprintf('area%s',commAmount$area)
  commAmount$fleet <- 'comm'
  commAmount <- commAmount[c('year','step','area','fleet','Freq','time')]
  names(commAmount)[5] <- 'catch.in.kilos'

  class(commAmount) <- c('Rgadget',class(commAmount))
  attr(commAmount,'formula') <- catch.in.kilos~time|area
  attr(commAmount,'plotGroups') <- ''
  attr(commAmount,'plotType') <- ''
  attr(commAmount,'xaxis') <- 'Year'
  attr(commAmount,'yaxis') <- 'Catch in kilos'
  attr(commAmount,'plotFun') <- 'xyplot'
  return(commAmount)
}
##' Plot the results from the summary functions of the Rgadget simulation.
##' @title Plot Rgadget
##' @param dat A Rgadget object
##' @author Bjarki Þór Elvarsson
plot.Rgadget <- function(dat){
  if(attr(dat,'plotFun')=='contour'){
    contourplot(attr(dat,'formula'),
                labels=FALSE,
                data=dat,
#                auto.key=list(),
                ylab=attr(dat,'yaxis'),
                xlab=attr(dat,'xaxis'),
                cuts=15,
                scales=list(x=list(rot=45),y=list(rot=45)))
    
  } else {
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

  ##  contourplot(total.catch~age+length|year,labels=FALSE,groups=step,data=alk[alk$fleet=='surv',],auto.key=list(),cuts=15,scales=list(x=list(rot=45),y=list(rot=45)))
}
