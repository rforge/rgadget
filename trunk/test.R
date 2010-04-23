source('stock.R')
source('function.R')
source('files.R')
source('gadgetfunctions.R')
source('mig6.R')
##' estimate fleet example
##'
##' Optimisation run starting from a random value.
##' Only estimate the multiplicative scaling factor for the commerical fleet
##'
##' The commands in this shell script:
##'
##'   - create the directory for output
##'   - do a gadget optimisation run (-l) 
##'            with the optimisation parameters in optinfofile (-opt) 
##'            output the estimated parameters to run1/params.out (-p)
##'
##'   - then do a simulation run to print the modelled population (-s)
##'      saving the likelihood scores to run1/like.out (-o)
##'
estimate.fleet <- function(){
  ## set up the initial folders
  try(system('mkdir estimate.fleet',intern=TRUE,ignore.stderr=TRUE))
  setwd('estimate.fleet')
  try(system('mkdir Gfiles',intern=TRUE,ignore.stderr=TRUE))
  ## example specific switches
  opt <- gadget.options()
  opt$numofareas <- 2
  opt$probarea <- c(1,1)/2
  opt$spbeta <- -0.3
  opt$H <- 400
  opt$m0 <- 1e-5
  opt$Fycomm <- 0.45
  opt$Fysurv <- 0.01
  opt$doescatchsurv <- 1:opt$numofareas
  opt$doescatchcomm <- 1:opt$numofareas
  opt$bylen <- 0
  opt$commfleettype <- 'linearfleet'
  opt$commmultiplicative <- '(* 0.1 #mult)'
  opt$calcindex <- 0
  opt$calcldist.c <- 0
  opt$calcldist.s <- 0
  opt$calc.in.kilos <- 1
  opt$estimate.recruits.by.year <- 0
  opt$optim.params <- 'mult'
  ## generate dataset
  sim <- Rgadget(opt)
  makefiles(sim$opt,sim)
  ## estimation run
  setwd('Gfiles')
  try(system('mkdir run1',intern=TRUE,ignore.stderr=TRUE))
  try(system('mkdir out',intern=TRUE,ignore.stderr=TRUE))
  callGadget(l=1,i='refinputfile',opt='optinfofile',p='run1/params.out')
  ## rerun from optimum to print population details
  callGadget(s=1,i='run1/params.out',o='run1/like.out')
  printfiles <- read.printfiles('out')
  ## return to old wd
  setwd('../../')
  return(printfiles)
}

estimate.growth <- function(){
  ## setting up the workspace
  try(system('mkdir estimate.growth',intern=TRUE,ignore.stderr=TRUE))
  setwd('estimate.growth')
  try(system('mkdir Gfiles',intern=TRUE,ignore.stderr=TRUE))
  ## example switches
  opt <- gadget.options()
  opt$numofareas <- 2
  opt$doescatchsurv <- 1:opt$numofareas
  opt$doescatchcomm <- 1:opt$numofareas
  opt$probarea <- c(1,1)/2
  opt$m0 <- 10^(-5)
  opt$H <- 4e3
  opt$Fysurv <- 0.01
  opt$Fycomm <- 0.7
  opt$survstep <- 1
  opt$calcindex <- 0
  opt$calcalk.c <- 0
  opt$calcalk.s <- 1
  ## create dataset
  sim <- Rgadget(opt)
  makefiles(sim$opt,sim)
  ## estimation run
  setwd('Gfiles')
  try(system('mkdir run1',intern=TRUE,ignore.stderr=TRUE))
  try(system('mkdir out',intern=TRUE,ignore.stderr=TRUE))
  callGadget(l=1,i='refinputfile',opt='optinfofile',p='run1/params.out')
  ## rerun from optimum to print population details
  callGadget(s=1,i='run1/params.out',o='run1/like.out')
}
  
standard1 <- function(){
  ## setting up the workspace
  try(system('mkdir standard1',intern=TRUE,ignore.stderr=TRUE))
  setwd('standard1')
  ## example switches
  opt <- gadget.options()
  opt$numofareas <- 2
  opt$doescatchsurv <- 1:opt$numofareas
  opt$doescatchcomm <- 1:opt$numofareas
  opt$probarea <- c(1,1)/2
  ## create dataset
  sim <- Rgadget(opt)
  opt1 <- sim$opt
  opt1$estimate.recruits.by.year.and.area <- 1
  try(system('mkdir Gfiles1',intern=TRUE,ignore.stderr=TRUE))
  makefiles(opt1,sim,location='Gfiles1')
  setwd('Gfiles1')
  try(system('mkdir run1',intern=TRUE,ignore.stderr=TRUE))
  try(system('mkdir out',intern=TRUE,ignore.stderr=TRUE))
##  callGadget(l=1,i='refinputfile',opt='optinfofile',p='run1/params.out')
  ## rerun from optimum to print population details
  callGadget(s=1,i='refinputfile',o='run1/like.out',log='run1/logfile')
  ## alternative formulation
  opt2 <- sim$opt
  opt2$estimate.recruits.by.year.constant.area <- 1
  setwd('..')
  try(system('mkdir Gfiles2',intern=TRUE,ignore.stderr=TRUE))
  makefiles(opt2,sim,location='Gfiles2')
  ## third formulation, recruitments are initialised slightly wrong
  opt3 <- opt2
  opt3$randomised.recruits <- 1
  try(system('mkdir Gfiles3',intern=TRUE,ignore.stderr=TRUE))
  makefiles(opt2,sim,location='Gfiles3')
  
}


##' Estimate Migration
##'
##' For Violeta Calian
migfunc <- function(){
  ## setting up the workspace
  try(system('mkdir migfunc',intern=TRUE,ignore.stderr=TRUE))
  setwd('migfunc')
  try(system('mkdir Gfiles',intern=TRUE,ignore.stderr=TRUE))
  ## example switches
  opt <- gadget.options()
  opt$numofareas <- 2
  opt$doescatchcomm <- 1:2
  opt$doescatchsurv <- 1:2
  f1 <- Vmig6(40,26,75,65,22,5,58,55)
  f2 <- Vmig6(22,5,58,55,40,26,75,65)
  opt$migrationP1 <- rep(1-f2,4)
  opt$migrationP2 <- rep(f1,4)
  opt$murec <- 13
  opt$probarea <- c(1,1)/2
  opt$diffusion <- 0.01
  opt$driftx <- 0.01
  opt$drifty <- 0.01
  opt$lambda <- 1
  opt$doesfuncmigrate <- 1
  opt$bylen <- 1
  opt$optim.params <- c('diffusion','driftx','drifty')
  opt$estimate.recruits.by.year <- 0
  
  sim <- Rgadget(opt)
  makefiles(sim$opt,sim)
  filefuncmigrate(sim$opt,sim,recdata=c(22,5,58,55,40,26,75,65))
  ## estimation run
  setwd('Gfiles')
  try(system('mkdir run1',intern=TRUE,ignore.stderr=TRUE))
  try(system('mkdir out',intern=TRUE,ignore.stderr=TRUE))
  callGadget(l=1,i='refinputfile',opt='optinfofile',p='run1/params.out')
  ## rerun from optimum to print population details
  callGadget(s=1,i='run1/params.out',o='run1/like.out')
  printfiles <- read.printfiles('out')
  setwd('../../')
  return(printfiles)  
}

