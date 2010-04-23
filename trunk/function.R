#' Gagdet options
#'
#' This function creates a list of default values of all necessary switches
#' for the RGadget simulation and file output. The user can then change the
#' values of the switches and use the changed list as input to RGadget.
#' @return a list of swithes
#' \item{stocks}{names of the stocks in the simulation}
#' \item{doeseat}{Does the mature stock eat the immature}
#' \item{doescatchsurv}{Is there a survey}
#' \item{doescatchcomm}{Is there a commercial effort}
#' \item{doesmigrateimm}{Does the immature stock migrate}
#' \item{doesmigratemat}{Does the mature stock migrate}
#' \item{migrationP1}{Probability of staying in area 1 (TWO AREA ASSUMPTION)}
#' \item{migrationP2}{Probability of staying in area 2 (TWO AREA ASSUMPTION)}
#' \item{doesfuncmigrate}{(migration) pde's used to describe migration.}
#' \item{diffustion}{(migration) diffusion parameter}
#' \item{driftx}{(migration) drift in x coordinate}
#' \item{drifty}{(migration) drift in y coordinate}
#' \item{doesmove}{Does the immature stock mature into the mature stock}
#' \item{numofareas}{Number of gadget areas}
#' \item{probarea}{A vector of proportions in a given area, assumed equal for both stocks}
#' \item{areasize}{Size of the gadget area (assumed equal for all areas}
#' \item{area.temperature}{Average temperature of the area}
#' \item{immminage}{Minimum age of the immmature stock}
#' \item{immmaxage}{Maximum age of the immmature stock}
#' \item{matminage}{Minimum age of the mature stock}
#' \item{matmaxage}{Maximum age of the mature stock}
#' \item{minlen}{Minimum length of both stocks}
#' \item{maxlen}{Maximum length of both stocks}
#' \item{lengthgrouplen}{Size of each lengthgroup. We assume the size of the lengthgroups is the same for both stocks.}
#' \item{a}{a in the length-weight relationship a*l^b}
#' \item{b}{b in the length-weight relationship a*l^b}
#' \item{sigma}{The standard deviation of length at i years old. This vector must the same length as the number of ages.}
#' \item{murec}{If specified this will be the meanlength of recruits}
#' \item{lsup}{L-infinity. Bertalanffy growth parameters lsup, and k for the growth function (used for all ages > 1)}
#' \item{binn}{binn is the maximum updating length}
#' \item{beta}{Beta for beta-binomial}
#' \item{numobs}{number of years observed}
#' \item{numoftimesteps}{number of timesteps in each year}
#' \item{z}{z is the natural mortality constant used to calculate the size of the initial population for age 2 +}
#' \item{spalpha}{alpha for the predation suitability function}
#' \item{spbeta}{beta for the predation suitability function}
#' \item{spagamma}{gamma for the predation suitability function}
#' \item{spdelta}{delta for the predation suitability function}
#' \item{m0}{m0 for the maximum consumption}
#' \item{m3}{m3 for the maximum consumption}
#' \item{H}{H for the maximum consumption}
#' \item{otherfrac}{the fraction of otherfood that is eaten}
#' \item{otherfood}{maxratioconsumed}{The maximum portion consumed, in Gadget it is 0.95, this is known as understaocking in Gadget}
#' \item{survstep}{timestep for the survey}
#' \item{salphasurv}{for the suitability function - survey}
#' \item{sbetasurv}{for the suitability function - survey}
#' \item{survfleettype}{Fleettype for the survey}
#' \item{survmultiplicative}{For the fleettype}
#' \item{Fysurv}{Fishing effort of the survey}
#' \item{salphacomm}{for the suitability function - commerical catch}
#' \item{sbetacomm}{for the suitability function - commercial catch}
#' \item{commfleettype}{Fleettype for the commercial catch}
#' \item{commmultiplicative}{For the fleettype}
#' \item{Fycomm}{Fishing effort of the commercial catch}
#' \item{calcindex}{output survey index likelihood file}
#' \item{calc.in.kilos}{output catches in kilos likelihood file}
#' \item{estimate.recruits.by.year}{should recruits be estimated by year}
#' \item{estimate.recruits.by.year.and.area}{should recruits be estimated by year and area}
#' \item{estimate.recruits.by.year.constant.area}{should recruits be estimated by year with constant by area effect (fewer degrees of freedom used compared to the one above).}
#' \item{randomised.recruits}{Should recruits be initialised randomly}
#' \item{bylen}{output survey index likelihood file by length groups, 0 for age aggregation and 1 for length aggregation}
#' \item{length.groups}{length groups for indices}
#' \item{calcldist.c}{output length distribution likelihood data for the catch}
#' \item{calcalk.c}{output age length distribution likelihood data for the catch}
#' \item{calcldist.s}{output length distribution likelihood data for the survey}
#' \item{calcalk.s}{output age length distribution likelihood data for the survey}
#' \item{file.migration.ratio}{Output migration matricies (0) or ratios (1)}
#' \item{alkagg}{aggregation level for age length distributions, will be used for both}
#' \item{survey.sigma}{sigma for error in survey indices}
#' \item{catch.sigma}{sigma for error in catchdistributions}
#' \item{frac}{fraction of catch that should be put in likelihood files}
#' \item{doesgrow}{do the stocks grow}
#' \item{growthfunction}{What growth function should be used, currently only lengthvbsimple is implemented}
#' \item{optim.params}{What parameters should Gadget optimise}
gadget.options <- function(){
  opt <- list(
#############################################################
#
# This file contains all variables and switches
# for a 2 stock 1 area Gadget model.
#
              stocks=c('imm','mat'),
           
#############################################################
#
# Set the switch to 1 if you want to catch, else set it to 0
#
              doeseat=1,
              
              doescatchsurv=1,
              doescatchcomm=1,
# Migration
              doesmigrateimm = 1,
              doesmigratemat = 1,
              migrationP1 = c(1,0.6,0.6,1),
              migrationP2  = c(0.6,1,1,0.6),
              doesfuncmigrate = 0,
              diffusion = NULL,
              driftx = NULL,
              drifty = NULL,
              lambda = NULL,
              
# immature fish move into mature population
              
              doesmove=1,

# Number of areas (cannot currently be more than 2)
              numofareas = 1,
#The portion of the stock in area 1.
              probarea=1,
# The temperature of the area
              area.temperature=5,
# areas assumed to be of equal size
              areasize=2*10^5,

#################################
#
# Variables for the ages
#
# Min- and maxage for immature
              immminage=1,
              immmaxage=3,
# Min- and maxage for the mature
              matminage=4,
              matmaxage=10,

#################################
#
# Variables for the lengths and weights
#
#min and max length in lengthgroups
              minlen=5,
              maxlen=90,

# Size of each lengthgroup. We assume the size of
# the lengthgroups is the same for both stocks.
              lengthgrouplen=1,

# a and b in the length-weight relationship a*l^b
              a=10^(-5),
              b=3,

# The standard deviation of length at i years old
# This vector must the same length as the number of ages.
              sigma=c(2.2472, 2.8982, 4.0705, 4.9276,
                5.5404, 5.8072, 6.0233, 8, 9, 9),
# Meanlength for the recruits
              murec=NULL,
# von Bertalanffy growth parameters for the growth function
# (used for all ages > 1)
# L-infinity
              lsup=115,
# k
              k=0.09,

# binn is maximum updating length
              binn=15,

# Beta for beta-binomial
              beta=200,

#################################
# Variables for time and area
#
# number of years observed
              numobs=10,

# numoftimesteps is a number of timesteps in each year
              numoftimesteps=4,


#################################
#
# Variables for the the initial stock
#

# z is the natural mortality constant used to calculate the size of
# the initial population for age 2 +
              z=0.2,


#################################
#
# Variables for the predation
#

# alpha and beta for the predation suitability function
              spalpha = 4.5,
              spbeta = -0.2,
              spgamma = -0.3,
              spdelta = 1,

# For the maximum consumption
              m0=10^(-2),
              m3=3,
              H=4*10^3,

# otherfrac is the fraction of otherfood that is eaten
              otherfrac=0.8,
              otherfood=50000,
# The maximum portion consumed, in Gadget it is 0.95
# this is known as understaocking in Gadget
              maxratioconsumed=0.95,

#################################
#
# Variables for the catch
#
              
# timestep for the survey
              survstep = 2,

#alpha and beta for the suitability function - survey
              salphasurv= -4.5,
              sbetasurv= 0.3,
              survfleettype='totalfleet',
              survmultiplicative='1',
              Fysurv=0.1,
#alpha and beta for the suitability function - catch
              salphacomm = -8,
              sbetacomm =  0.22,
              commfleettype='totalfleet',
              commmultiplicative='1',
              Fycomm=0.7,
#################################
#
# Gadget likelihood files
# used when running gadget -l
# ?should file options be here?

# output survey index likelihood file
# this calculates 2 indices for each area
# one for recruits and another representing all age2+ fish
              calcindex = 1,
              calc.in.kilos = 0,
                                        # should recruits be estimated
              estimate.recruits.by.year=1,
              estimate.recruits.by.year.and.area=0,
              estimate.recruits.by.year.constant.area=0,
              randomised.recruits=0,
              
# output survey index likelihood file by length groups
# 0 for age aggregation
# 1 for length aggregation
              bylen = 1,
# length groups for indices
# only 2 allowed - unless you edit files.splus
# filesurveyindexlen()
# filelikelihood()
# fileaggregation()
              length.groups=c(4,16,90),
# output length distribution likelihood data for the catch
              calcldist.c=1,
# output age length distribution likelihood data for the catch
              calcalk.c=1,

# output length distribution likelihood data for the survey
              calcldist.s=1,
# output age length distribution likelihood data for the survey
              calcalk.s=1,
# output migration matricies or ratios
              file.migration.ratio=0,
# aggregation level for age length distributions
# will be used for both
              alkagg = 5,

#########################
#
# lognormal error can be added to the likelihood data
#

# sigma for error in survey indices
              survey.sigma=0,
# sigma for error in catchdistributions
              catch.sigma=0,

# fraction of catch that should be put in likelihood files
              frac=0.1,
# growthfunction
              doesgrow=1,
              growthfunction='lengthvbsimple',
# optimiser
              optim.params=''
              )
  return(opt)
}
#' Derived options
#'
#' This function is a helper function for RGadget and all file export functions,
#' it calculates all additional options and switches that can be derived from
#' the gadget options lists.
#' @param opt gadget options list
#' @return augmented gadget options list
derivedOptions <- function(opt){
  
# Min- and maxage over both of the stocks
    
  opt$minage <- min(opt$immminage,
                    opt$matminage)
  opt$maxage <- max(opt$immmaxage,
                    opt$matmaxage)
  
#################################
# Variables for time and area
#
# lengthoftimesteps is the length of each timestep
  opt$lengthoftimesteps <- 12/opt$numoftimesteps
#################################
#
# Variables for the the initial stock
#

# n is a vector where n[i] is number of recruits in timestep i
# mod(numoftimestep) in year ([i/numoftimesteps]+1)
  opt$n <- rep(c(1000000,0,0,0),opt$numobs)
  
    
#################################
#
# Natural mortality
#
# mort[i] is the natural mortality for age i at each time step
# this assumes the same natural mortality as for the initial year
# and for each age
  opt$mort <- c(rep(opt$z,opt$maxage-1),0.5)

#################################
#
# Variables for the predation
#
# Otherfood, the amount eaten in biomass
  opt$otherfood <- rep(opt$otherfood,
                       opt$numobs*opt$numoftimesteps*opt$numofareas)
  dim(opt$otherfood) <- c(opt$numofareas,opt$numobs*opt$numoftimesteps)
#################################
#
# Variables for the catch
                                        #

    #F for the catch
  opt$Fysurv <- rep(opt$Fysurv, opt$numobs/length(opt$Fysurv))
  opt$Fycomm <- rep(opt$Fycomm, opt$numobs/length(opt$Fycomm))

  #################################
#
# Initialises the times

# Length of the timestep
  opt$dt <- 1/opt$numoftimesteps

################################
#
# Initialises the ages
#
# Dummy variables
  opt$matmax <- opt$matmaxage-opt$matminage+1
  opt$immmax <- opt$immmaxage-opt$immminage+1

################################
#
# Initialises the lengths and the weights
#
# l is a vector containing the endpoints for the lengthgroups.
# We assume that the size of the lengthgroups are all the same,
# so if this is not preferable change next line of code.
  opt$l <- seq(opt$minlen,opt$maxlen,opt$lengthgrouplen)

# The number of lengthgroups
  opt$numoflgroups <- length(opt$l)-1

#lt[i] is the mean of lengthgroup i
  opt$lt <- (opt$l[2:length(opt$l)]+opt$l[1:(length(opt$l)-1)])/2

#w[i] is the mean weight in lengthgroup i
  opt$w <- opt$a*opt$lt^opt$b

# mu[i] meanlength for i years old
  opt$mu <- opt$lsup*(1-exp(-opt$k*1:opt$maxage))
  if(!is.null(opt$murec))
    opt$mu[1] <- opt$murec
# timesteps of the survey
  opt$survtimesteps <- (1:opt$numobs-1)*opt$numoftimesteps+opt$survstep
#################################
#
# Predation
#
# Maximum consumption
  opt$maxConsumption <- opt$m0*opt$lt^opt$m3*12*opt$dt
  if(opt$numofareas==1){
    opt$doesmigratemat <- 0
    opt$doesmigrateimm <- 0
  }
  
  return(opt)
}

#' Adjust for overconsumption 
#'
#' For each prey an upper limit needs to be set on the total amount
#' consumed by all predators so as not to obtain more consumption than
#' available biomass.  Consumption is limited to 95\% ($R_M$) of the available
#' biomass. This is implemented by scaling target consumption by all
#' predators. 
#' Let R_{prey}(l) be the Ratio consumed and R_M be the Maximum Ratio Consumed
#' Then
#' \deqn{R_{prey}(l)=\frac{\sum_{pred}\sum_{L}C_{pred,prey}(L,l)}{N_{prey}(l)W_{prey}(l)}}
#' If R_{prey}(l)>R_M consumption is adjusted as follows
#' \deqn{C_{pred,prey}(L,l)=R_MN_{prey}(l)W_{prey}(l)\frac{C_{pred,prey}(L,l)}{\sum_{pred}C_{pred,prey}(L,l)}}
#' @param C is the commercial catch of prey
#' @param S is the survey catch of prey
#' @param E is the consumption of prey by predator
#' @param N is the total number of prey
#' @param opt gadget options list
#' @return a list with adjusted catches/consumption for C, S and E.
adjustconsumption <- function(C,
                              S=NULL,
                              E=NULL,
                              N,
                              opt)
{
  if(is.null(S))
    S <- array(0,dim(C))
  if(is.null(E))
    E <- array(0,dim(C))
  ## Nasty hack
  if(opt$numofareas==1){
    dim(C) <- c(1,dim(C))
    dim(S) <- c(1,dim(S))
    dim(E) <- c(1,dim(E))
    dim(N) <- c(1,dim(N))
  }
  ratio <- apply(C+S+E,c(1,2),sum)/apply(N,c(1,2),sum)
  ratio <- ifelse(is.infinite(ratio)|is.nan(ratio),0,ratio)
  index <- ratio > opt$maxratioconsumed
  if(sum(index)>0)
    print("Warning - understocking has occured")
  index2 <- array(index,c(dim(index),dim(C)[3]))
  C[index2] <- (opt$maxratioconsumed/ratio[index])*C[index2]
  S[index2] <- (opt$maxratioconsumed/ratio[index])*S[index2]
  E[index2] <- (opt$maxratioconsumed/ratio[index])*E[index2]
  return(list(C=C,S=S,E=E))
}

#' Fleet catches
#'
#' Catch is implemented in \texttt{R} using the {\it Linearfleet}
#' option in Gadget. 
#' Let \eqn{C_{fleet,prey}(l,a,t)} be the number of age \eqn{a} prey, in
#' lengthgroup \eqn{l} caught at timestep \eqn{t}, then
#' \deqn{C_{fleet,prey}(l,a,t) = F_{l,t}N_{prey}(l,a,t)\Delta t}
#' with \eqn{F_{l,t} = S_{l}F_{y}} where \eqn{F_y} is constant for each year,   
#' \deqn{S_{l} = \frac{1}{1+e^{-\alpha-\beta l}}}
#' is the suitability function and \eqn{\alpha} and \eqn{\beta} are constants.  
#' @param N number of prey
#' @param timestep the timestep of the catch
#' @param Fy fishing yield
#' @param salpha suitability constant for the fleet
#' @param sbeta suitability constant for the fleet
#' @param numperyear number of catch timesteps
#' @param opt gadget options list
#' @return Total catches of the fleet
catch <- function(N,
                  timestep,
                  Fy,
                  salpha,
                  sbeta,
                  numperyear,
                  opt)
{
  #The suitability for the catch
  temp<-suitability(salpha,sbeta,0,1,opt$lt)
  Sl<-temp[1,]
  Fy <- rep(Fy/numperyear,each=opt$numobs)
  
  #Proportion caught each year
  
  Fly<-Sl*Fy[timestep]
  if(length(dimnames(N)$area)>0)
    Fly <- rep(Fly,each=dim(N)[1]) 
  C<-Fly*N
  
  return(C)
}

#' Length distribution
#'
#' This is a helper function for the firststep function. This defines the
#' length distribution for each age group
#' @param mu mean length for all ages
#' @param sigma standart deviation of length for all ages
#' @param l lengthgroups
#' @return a matrix of dimension length(mu) X (length(l)-1)
distr <- function(mu,sigma,l)
{
  fi <- (pnorm(rep(l[-1],each=length(sigma)),mu,sigma)-
         pnorm(rep(l[-length(l)],each=length(sigma)),mu,sigma))
  dim(fi) <- c(length(sigma),length(l)-1)
  fi[,1] <- pnorm(rep(l[2],length(sigma)),mu,sigma)
  fi[,length(l)-1] <- (1-pnorm(rep(l[length(l)-1],
                                   length(sigma)),mu,sigma))

  return(t(fi))
}

#' Eat
#'
#' The following variables are used in the consumption calculations:
#' l is Lengthgroup in prey
#' L is Lengthgroup in predator
#' A is Areasize
#' H The density (biomass per area unit) of available food at
#' which the predator can consume half maximum consumption
#' \eqn{\Delta t} as Length of timestep
#' \eqn{M_{pred}(L)} as Maximum consumption
#' \eqn{\psi_{pred}(L)} as Fraction of \eqn{M_{pred}} consumed
#' N_{pred}(L) as Number of predator \eqn{pred} in lengthgroup $L
#' N_{prey}(l) as Number of prey \eqn{prey} in lengthgroup l
#' W_{prey}(l) as The mean weight of prey of length l
#' S_{pred,prey}(L,l) as Suitability of prey at length l for pred at length L
#' C_{prey,pred}(L,L) as Total weight predator of length L consumes of prey of length $l$
#' The formula for the consumption is as follows:
#' \deqn{C_{pred,prey}(L,l)&=N_{pred}(L)M_{pred}(L)\Psi_{pred}(L)\frac{F_{pred,prey}(L,l)}{\sum_{l,prey}F_{pred,prey}(L,l)}}
#' \deqn{=N_{pred}(L)M_{pred}(L)\frac{\sum_{l,prey}F_{pred,prey}(L,l)}{\sum_{l,prey}F_{pred,prey}(L,l)+HA}\frac{F_{pred,prey}(L,l)}{\sum_{l,prey}F_{pred,prey}(L,l)}}
#' \deqn{=N_{pred}(L)M_{pred}(L)\frac{F_{pred,prey}(L,l)}{\sum_{l,prey}F_{pred,prey}(L,l)+HA}}
#' where
#' \deqn{F_{pred,prey}(L,l) &=S_{pred,prey}(L,l)N_{prey}(l)W_{prey}(l)}
#' \deqn{ M_{pred}(L) &=m_0e^{(m_1T-m_2T^3)}L_{pred}^{m_4}\Delta t}
#' The suitability function for predation used in the \R model is:
#' \deqn{S_{pred,prey}(L,l) = \frac{\delta}{1+e^{-\alpha-\beta l-\gamma L}}}
#' With one predator, one prey and otherfood the equation becomes:
#' \deqn{C_{L,l}&=N_{L}M_{L}\Psi_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA}}
#' \deqn{=N_{L}M_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA+HA}}
#' where O is the density of otherfood.
#' @param PreyInArea Number of prey items in the area
#' @param PredInArea Number of predator items in the area
#' @param step the timestep, that is the time of the year
#' @param opt gadget options list
#' @return The total unadjusted consumption of the predator on the prey
eat <- function(PreyInArea,PredInArea,step,opt){  
  preydim <- dim(PreyInArea)
  preddim <- dim(PredInArea)
  if(preydim[2]!=preddim[2]){
    print("Error - the number of lengthgroups is not the same for predation")
  }else{
    numoflgroups <- preydim[2]
  }
  ## The amount eaten
  Eat<-array(0,preydim[1:3])
  ## The suitability for the predation
  Spred<-suitability(opt$spalpha,opt$spbeta,opt$spgamma,opt$spdelta,opt$lt,opt$lt)
  ## Food = S(L,l)*N_l
  Food<-array(0,c(numoflgroups,opt$numoflgroups,preydim[3]))
  
  for(area in 1:opt$numofareas){
    Prey <- PreyInArea[area,,,step]
    Pred <- PredInArea[area,,,step]
  # F_{Llat}
    for(i in 1:preydim[3])
      Food[,,i]<-t(Spred)*Prey[,i]*opt$w
  # loop through predators lengths
    predsum <- apply(Pred,1,sum)*opt$maxConsumption
    foodsum <- apply(Food,2,sum)
    other <- opt$H*opt$lengthoftimesteps+opt$otherfood[step]*opt$otherfrac
    for(j in 1:numoflgroups){
      Eat[area,,] <- Eat[area,,] +
        predsum[j]*Food[,j,]/(foodsum[j] + other*opt$areasize)
    }

    Eat[area,,] <- Eat[area,,]/opt$w
  }
  return(Eat)
}

#' The first timestep
#'
#' The simulation is started with an initial population, the age of which
#' ranges from the minimum age of the immature stock to the maximum age
#' of the mature stock. This population is calculated as follows:
#' Let \eqn{n_{a}} be the number of age \eqn{a} individuals in the first
#' timestep, \eqn{\mu_{a}} the mean length at age \eqn{a} and \eqn{\sigma_{a}}
#' the standard deviation of length at age \eqn{a}. For the minimum age
#' (representing recruitment) see the recruitment function. For a given
#' constant mortality \eqn{Z_{0}} we get
#' \deqn{n_{a} = n_{a-1}e^{-Z_{0}}}
#' to calculate the number at age for all \eqn{a}.
#' The number in lengthgroup $i$ at age $a$ in timestep 1 can then be
#' calculated from:
#' \deqn{N_{i,a,1} = n_a\left(\Phi\left(\frac{l_{i+1}-\mu_a}{\sigma_a}\right)-\Phi\left(\frac{l_{i}-\mu_a}{\sigma_a}\right)\right)}
#' where \eqn{l_{i}} and \eqn{l_{i+1}} are the endpoints of lengthgroup \eqn{i},
#' \eqn{N_{l,a,t}} is the number at age \eqn{a} in lengthgroup \eqn{l} at
#' timestep \eqn{t} and \eqn{\Phi} is the probability function for Normal
#' distribution. 
#' NB: in Gadget (which is programmed in C++) the value of $\Phi$ is
#' approximated whereas \texttt{R} uses integration. To ensure
#' compatibility between the models, the initial population for Gadget is
#' entered directly from the initial population file rather than
#' calculated from a Normal distribution. While this is an option
#' within Gadget, it is not the standard method.
#' @param n
#' @param mu
#' @param sigma
#' @param l
#' @param z
#' @param numofareas
#' @param probarea
#' @param minage
#' @param maxage
#' @return matrix with initial distribution
firststep <- function(n,
                      mu,
                      sigma,
                      l,
                      z,
                      numofareas,
                      probarea,
                      minage,
                      maxage
                      )
{
  minlen <- min(l)
  maxlen <- max(l)
  numoflgroups <- length(l)-1
  num.agegroup <- n[1]*exp(-(0:(maxage-1))*z)
  
  if(maxage!=length(sigma)) {
    print("Error - the number of age groups for sigma doesnt match the maximum age")
    return('Error')
  }
    
  temp <- distr(mu,sigma,l)*rep(num.agegroup,each=numoflgroups)
  if(minage>1)
    temp[,1:minage] <- 0 
  # assign the distribution to areas according to the probability of being in
  # that area
  if(length(probarea)!=numofareas){
    print("Error - the area probabilities do not match the number of areas")
    return('Error')
  }
  initial.distribution <- array(rep(temp,each=numofareas),
                                c(numofareas,dim(temp)))*probarea
  dimnames(initial.distribution) <- list(area=1:numofareas,
                                         length=minlen:(maxlen-1),
                                         age=minage:maxage
                                         )
  
  return(initial.distribution)
}

#' Growth probability
#'
#' Growth is according to a von Bertalanffy equation 
#' \deqn{\mu_{a} = L_{\infty}(1-e^{-\kappa a})}
#' with the lengthvbsimple growth function from Gadget implemented.
#' For a fish of age a and length l, mean length growth \eqn{\Delta L} is
#' then calculated as:
#' \deqn{\Delta L =L_{\infty}(1 - \frac{l}{L_{\infty}})(1 - e^{-\kappa \Delta t})}
#' The length distribution is updated using the beta-binomial
#' distribution, ie the probability of growing x lengthgroups, given
#' maximum lengthgroupgrowth n, is
#' \deqn{P[X = x] =  \frac{\Gamma(n+1)}{\Gamma(n-x+1)\Gamma(x+1)} \frac{\Gamma(\alpha + \beta)}{\Gamma(n+\alpha+\beta)} \frac{\Gamma(n-x+\beta)}{\Gamma(\beta)} \frac{\Gamma(x+a)}{\Gamma(\alpha)}}
#' with \eqn{\alpha = \frac{\beta\Delta L}{n-\Delta L}} to preserve the mean
#' lengthgrowth according to equation (\ref{deltaL}). NB: the
#' expected value of \eqn{\Delta L} should be taken into consideration when
#' fixing n. 
#' Let \eqn{G = [g_{ij}]} be the length updating matrix where \eqn{g_{ij}} is the
#' probability of growing from lengthgroup i to lengthgroup j
#' obtained from the equation above.
#' \deqn{N_{l,a+1,t+\Delta t} = \sum_{l'\leq l}g_{l'l}N_{l,a,t}}
#' with \eqn{N_{l,a,t}} as described for the initial population for a >
#' min a .
#' @param lt
#' @param beta
#' @param lsup
#' @param k
#' @param dt
#' @param lengthgrouplen
#' @param binn
#' @return a matrix where the index (j,i) repsents the probability of going
#' lengthgroup i to lengthgroup j
growthprob <-function(lt,
                      beta,
                      lsup,
                      k,
                      dt,
                      lengthgrouplen,
                      binn)
{
  
  prob <- function(alpha,beta,x){
    na <- length(alpha)
    n <- length(x) - 1
    alpha <- rep(alpha,n + 1)
    x <- rep(x,each=na)
    ## Create a probability matrix where the columns represent the
    ## probability of growing x lengthgroups for each lengthgroup
    ## length group jumps are distributed according to a beta-binomial
    ## distribution
    val <- exp(lgamma(n + 1)+
               lgamma(alpha + beta) +
               lgamma(n - x + beta) +
               lgamma(x + alpha) -
               lgamma(n - x + 1) -
               lgamma(x + 1) -
               lgamma(n + alpha + beta) -
               lgamma(beta) -
               lgamma(alpha))
    dim(val) <- c(na,n + 1)
    growth.matrix <- array(0,c(na,na))
    for(lg in 1:na){
      if(lg == na){
        growth.matrix[na,na] <- 1
      } else if(lg + n > na){
        growth.matrix[lg,lg:(na-1)] <- val[lg,1:(na - lg )]
        growth.matrix[lg,na] <- sum(val[lg,(na - lg + 1):(n + 1)])
      } else {
        growth.matrix[lg,lg:(n + lg)] <- val[lg,]
      }
    }
    return(growth.matrix)
  }
  ## dmu[i] is the mean growth in lengthgroup i 
  dmu <- lsup*(1-lt/lsup)*(1-exp(-k*dt))

  ## if growth>maxgrowth assume that growth is a bit smaller than maxgrowth
  dmu[dmu/lengthgrouplen >= binn] <- binn-0.1

  ## if growth is negative assume no growth
  dmu[dmu < 0] <- 0
  
  alpha <- (beta*dmu/lengthgrouplen)/(binn-dmu/lengthgrouplen)
  ## possible length growth
  length.growth <- 0:binn
  
  return(prob(alpha,beta,length.growth))
}

#' Migration
#'
#' Migration takes place in every timestep and migration is assumed to be
#' constant for all years.
#' Migration at timestep $t$ is defined as a $2\times 2$ transition
#' matrix $P_t := [p_{ij}]$ where $p_{ij}$ is the proportion moving from
#' area $j$ to area $i$. {\bf NB:} for $P$ to be a transition matrix
#' $\sum_ip_{ij} = 1$, for a fixed $j$.  
#' If $N\textrm{\emph{1}}_t$ is a matrix containing the abundance in
#' numbers in area 1 at timestep $t$ before migration and
#' $N\textrm{\emph{2}}_t$ is the same number for area 2, the numbers
#' after migration will be
#' \begin{align}
#' N\textrm{\emph{1}}_t &= p_{11}\cdot N\textrm{\emph{1}}_t + p_{12}\cdot N\textrm{\emph{2}}_t \notag \\
#' N\textrm{\emph{2}}_t &= p_{21}\cdot N\textrm{\emph{1}}_t + p_{22}\cdot N\textrm{\emph{2}}_t 
#' \end{align}

#' As there are only 2 areas and the sum of the transition matrix columns
#' must be 1 it is only necessary to enter the values of the first line
#' into the migration file in the \texttt{R} program.  In the
#' Gadget2.0.03 migration file, the proportion moving must be removed
#' from the old area and added to the new area.
#' @param N1 number in area 1
#' @param N2 number in area 2
#' @param k time step
#' @param P migration matrix
#' @param opt gadget options list
#' @return a matrix with the new population after migration.
migrate <- function(N1,N2,k,P=migrationProb(),opt=gadget.options())
{
#
# Check if P is a transition matrix
  if(max(P)>1)
    print("Error - migration matrix is not a transition matrix")

  d1<-dim(N1)
  d2<-dim(N2)
  if(d1[1]!=d2[1] && d1[2]!=d2[2])
    print("Error - migration matrices do not have the same dimensions")

  New<-array(0,c(d1[1],d1[2],opt$numofareas))
  # Use the advantage of only having 2 areas
  New[,,1]<-P[1,1,k]*N1+P[1,2,k]*N2
  New[,,2]<-(1-P[1,1,k])*N1+(1-P[1,2,k])*N2
  return(New)
}

#' Migration matrix
#'
#' Helper function for migrate
#' @param opt gadget options list
#' @return Migration array
migrationProb <- function(opt=gadget.options()){
  P <- array(rbind(opt$migrationP1,1-opt$migrationP2),
             c(1,opt$numofareas,opt$numoftimesteps))
  return(P)
}

#' Recruitment
#'
#' The timestep (or timesteps) on which recruitment takes place is
#' defined by the user. 
#' Given $n_{t}$ recruits, at timestep $t$, with mean length $\mu$ and
#' standard deviation of length $\sigma$, the number of recruits in
#' lengthgroup $i$ is calculated by:
#' \deqn{N_{i,1,\text{t}} = n_t(\Phi(\frac{l_{i+1}-\mu}{\sigma})-\Phi(\frac{l_{i}-\mu}{\sigma}))}
#' As for the initial population, the number of recruits in each length
#' groups is given in the recruit input file (cf page \pageref{first}). 
#' @param n
#' @param mu
#' @param sigma
#' @param l
#' @param numofareas
#' @param probarea
#' @param numobs
#' @param numoftimesteps
#' @return Recruits by area, length, time
recruits <- function(n,mu,sigma,
                     l,numofareas,probarea,
                     numobs,numoftimesteps)
{
  
  Temp <- distr(mu,sigma,l)%*%t(n)
  rec <- array(rep(Temp,each=2),c(numofareas,dim(Temp)))*probarea
  dimnames(rec) <- list(area = 1:numofareas,
                        length = min(l):(max(l)-1),
                        time=paste(sprintf('Year_%s',rep(1:numobs,
                                each=numoftimesteps)
                                ),
                                sprintf('Step_%s',rep(1:numoftimesteps,
                                                      numobs)),
                                sep='_'))
  return(rec)
}

#' Prey suitability
#'
#' The suitability function for predation used in the {\texttt R} model is:
#' \deqn{S_{pred,prey}(L,l) = \frac{\delta}{1+e^{-\alpha-\beta l-\gamma L}}}
#' With one predator, one prey and otherfood the equation becomes:
#' \deqn{C_{L,l}&=N_{L}M_{L}\Psi_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA}}
#' \deqn{=N_{L}M_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA+HA}}
#' where $O$ is the density of otherfood.
#' @param salpha
#' @param sbeta
#' @param sgamma
#' @param sdelta
#' @param l prey length group(s)
#' @param L predator length group(s)
#' @return matrix of suitabilities, columns prey length, lines predator length
suitability <- function(salpha,
                        sbeta,
                        sgamma,
                        sdelta,
                        l,
                        L=c(0))
{
  
  S <- array(sdelta/(1+exp(-(salpha+sbeta*rep(l,each=length(L))+
                             sgamma*rep(L,length(l))))),
             c(length(L),length(l)))
  dimnames(S) <- list(sprintf('Pred_length_%s',L),
                      sprintf('Prey_length_%s',l))
  return(S)
}
