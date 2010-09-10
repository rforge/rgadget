##' Each year is divided into timesteps of equal length.  Two stocks are
##' simulated, representing immature and mature stock components, which
##' live on 2 areas. Each stock grows and is subject to natural mortality
##' and there is recruitment into the immature stock. Optional processes are:
##' - 0 to 2 fleets, representing the commercial fleet and a survey.
##' - Migration.
##' - Movement between the stocks, representing maturation. 
##' - Consumption such that the mature stock is a predator of the immature stock.
##' Fleets are modelled as predators, as in Gadget. The survey takes place
##' in the first timestep of each each year and the commercial catch takes
##' place in each timestep for every year. Maturation from stock $A$ to
##' stock $B$ is modelled by moving the oldest agegroup of $A$ into $B$
##' with the age increasing if done on the last timestep of the year. This
##' replicates the Gadget process \emph{doesmove}.
##' The order of calculations is the same as in Gadget and is as follows:
##' 1. Migration between areas
##' 2. Consumption, including catch by the fleets
##' 3. Natural mortality
##' 4. Growth 
##' 5. Recruitment
##' 6. Remove the stock, here immature, that will move
##' 7. Increase the age
##' 8. Replace the stock, here to the mature, that has moved and increase the age. 
##' @title Rgadget
##' @param opt gadget options list
##' @return a list of arrays:
##' \item{Rec}{Recruits for all years}
##' \item{immStart}{Initial starting population for the immature stock age 2 and older}
##' \item{matStart}{Initial starting population for the mature stock age 2 and older}
##' \item{immNumRec}{Immature stock population for all timesteps, areas, ages and lengths}
##' \item{matNumRec}{Mature stock population for all timesteps, areas, ages and lengths}
##' \item{immCsurv}{Survey catches of the immature stock}
##' \item{matCsurv}{Survey catches of the mature stock}
##' \item{immCcomm}{Commercial catches of the immature stock}
##' \item{matCcomm}{Commercial catches of the mature stock}
##' \item{Eat}{Amount consumed of immatures by matures}
##' \item{GrowthProb}{Growthprobability matrix}
##' \item{immMort}{Natural mortality for the immature stock}
##' \item{matMort}{Natural mortality for the mature stock}
##' \item{opt}{Gadget options list used in the simulation}
Rgadget <- function(opt=gadget.options()){
  ## initialize the necessary variables  
  opt <- derivedOptions(opt)
  ## what areas are commercially exploited
  commAreas <- 1:opt$numofareas %in% opt$doescatchcomm
  ## define survey areas
  surveyAreas <- 1:opt$numofareas %in% opt$doescatchsurv

################################
#
# The stocks
# preN(k)=[N_{i,1,j}] is a matrix where immN(k)_{i,1,j} is the number
# of recruits in lengthgroup i at timestep j, area k
  immNumRec <- array(0,c(opt$numofareas,
                         opt$numoflgroups,
                         opt$immmax,
                         (opt$numobs*opt$numoftimesteps)))
  dimnames(immNumRec) <- list(area=1:opt$numofareas,
                              length=opt$minlen:(opt$maxlen-1),
                              age=opt$immminage:opt$immmaxage,
                              time=paste(sprintf('Year_%s',rep(1:opt$numobs,
                                each=opt$numoftimesteps)
                                ),
                                sprintf('Step_%s',rep(1:opt$numoftimesteps,
                                                      opt$numobs)),
                                sep='_'))
  matNumRec <- array(0,c(opt$numofareas,
                         opt$numoflgroups,
                         opt$matmax,
                         (opt$numobs*opt$numoftimesteps)))
  dimnames(matNumRec) <- list(area=1:opt$numofareas,
                              length=opt$minlen:(opt$maxlen-1),
                              age=opt$matminage:opt$matmaxage,
                              time=paste(sprintf('Year_%s',rep(1:opt$numobs,
                                each=opt$numoftimesteps)
                                ),
                                sprintf('Step_%s',rep(1:opt$numoftimesteps,
                                                      opt$numobs)),
                                sep='_'))

################################
#
# Calculates natural mortality per timestep

#preM is a survival matrix, same in both areas
  M <- diag(exp(-opt$mort*opt$dt))
  immMort <- M[opt$immminage:opt$immmaxage,
               opt$immminage:opt$immmaxage]
  matMort <- M[opt$matminage:opt$matmaxage,
               opt$matminage:opt$matmaxage]

################################
#
# Defines the catch matrices
  catch.switch<- opt$doescatchcomm+opt$doescatchsurv
  
  if(sum(catch.switch)>0)
    {
      immCcomm <- array(0,c(opt$numofareas,
                            opt$numoflgroups,
                            opt$immmax,
                            (opt$numobs*opt$numoftimesteps)))
      dimnames(immCcomm) <- dimnames(immNumRec)
      matCcomm <- array(0,c(opt$numofareas,
                            opt$numoflgroups,
                            opt$matmax,
                            (opt$numobs*opt$numoftimesteps)))
      dimnames(matCcomm) <- dimnames(matNumRec)
      immCsurv <- array(0,c(opt$numofareas,
                            opt$numoflgroups,
                            opt$immmax,
                            (opt$numobs*opt$numoftimesteps)))
      dimnames(immCsurv) <- dimnames(immNumRec)
      matCsurv <- array(0,c(opt$numofareas,
                            opt$numoflgroups,
                            opt$matmax,
                            (opt$numobs*opt$numoftimesteps)))
      dimnames(matCsurv) <- dimnames(matNumRec)
    }


#################################
#
# Predation
#

# The number eaten of immature by mature, the default is zero
  Eat <- array(0,c(opt$numofareas,
                   opt$numoflgroups,
                   opt$immmax,
                   opt$numobs*opt$numoftimesteps))
  dimnames(Eat) <- dimnames(immNumRec)
  
#################################

# Assume we have one pseudo stock which splits between the 2
# areas in the portion probarea1 in area 1 and
# (1-probarea1) in area 2.
  Start<-firststep(opt$n,
                   opt$mu,
                   opt$sigma,
                   opt$l,
                   opt$z,
                   opt$numofareas,
                   opt$probarea,
                   opt$minage,
                   opt$maxage)
  
# preStart[i,j] is number in pseudo stock at the beginning of timestep 1,
# (1 year olds are not included). We use Start to make initial
# mature and immature stock such that the amount of i year old in the
# mature/immmature stock is the same as in the pseudo stock.
  immStart <- Start[,,(opt$immminage+1):opt$immmaxage]
  matStart <- Start[,,(opt$matminage:opt$matmaxage)]
  if(opt$numofareas==1){
    ## ugly hack because of destroyed array dimension
    dimnames(immStart) -> tmp
    dim(immStart) <- c(1,dim(immStart))
    dimnames(immStart) <- list(area=1,length=tmp[[1]],age=tmp[[2]])
    dimnames(matStart) -> tmp
    dim(matStart) <- c(1,dim(matStart))
    dimnames(matStart) <- list(area=1,length=tmp[[1]],age=tmp[[2]])
  }
#Matrix of length divided recruits
  Rec <- recruits(opt$n,opt$mu[opt$immminage],opt$sigma[opt$immminage],
                  opt$l,opt$numofareas, opt$probarea,
                  opt$numobs,opt$numoftimesteps)

#G[i,j] is the probability of going from lengthgroup i to lengthgroup j
#Same in both areas
  G <- growthprob(opt$lt,
                  opt$beta,
                  opt$lsup,
                  opt$k,
                  opt$dt,
                  opt$lengthgrouplen,
                  opt$binn)

####################################
#  Calculations for all timesteps  #
####################################
  for(i in 1:(opt$numobs*opt$numoftimesteps))
    {
      num<-i%%opt$numoftimesteps
      if(num==0)
        num <- opt$numoftimesteps
      if(num!=1){      ############## if we are not in timestep 1  #########
        immNumRec[,,,i] <- immNumRec[,,,i-1]
        matNumRec[,,,i] <- matNumRec[,,,i-1]
      } else if(i==1){ ### we have a special update in the 1st timestep ###
        immNumRec[,,-1,1] <- immStart
        matNumRec[,,,1] <- matStart
      } else { ###### if we are in timestep 1 we have to update age ######
        
    #############
    # Age update
    # NOTE this is the last step of the
    # calculations done in previous timestep

    # Update age for immature upto age immmaxage-1
          immNumRec[,,-1,i] <- immNumRec[,,-opt$immmax,i-1]
    # Update age for mature
          matNumRec[,,-1,i] <- matNumRec[,,-opt$matmax,i-1]
    # Adding up the maxage ones
           matNumRec[,,opt$matmax,i] <-
             matNumRec[,,opt$matmax,i] + matNumRec[,,opt$matmax,i-1]

          if(opt$doesmove==1){
      ## Adding the ones which have moved between stocks
            matimmdiff <- opt$immmaxage-opt$matminage+2
            matNumRec[,,matimmdiff,i] <-
              matNumRec[,,matimmdiff,i]+immNumRec[,,opt$immmax,i-1]
          } else {
      ## if they don't move between stocks we have
      ## a + group for the immmaxage ones
            immNumRec[,,opt$immmax,i] <-
              immNumRec[,,opt$immmax,i]+immNumRec[,,opt$immmax,i-1]
          }
        }
      ############
      # Migration Assume only two areas atm
      if(opt$doesmigrateimm==1){
        immTemp<-migrate(immNumRec[1,,,i],immNumRec[2,,,i],
                         num,P=migrationProb(opt=opt),opt=opt)
        immNumRec[1,,,i] <- immTemp[,,1]
        immNumRec[2,,,i] <- immTemp[,,2]
      }
      if(opt$doesmigratemat==1){
        matTemp<-migrate(matNumRec[1,,,i],matNumRec[2,,,i],
                         num,P=migrationProb(opt=opt),opt=opt)
        matNumRec[1,,,i] <- matTemp[,,1]
        matNumRec[2,,,i] <- matTemp[,,2]
      }


  ############
  # Consumption calculations
      if(opt$doeseat==1){
          Eat[,,,i] <- eat(immNumRec,matNumRec,i,opt)
        }
      
  ############
  # Catch calculations
      if(num %in% opt$commstep){
        if(sum(opt$doescatchcomm) > 0)
          {
            if('imm' %in% opt$comm.catches){
              immCcomm[commAreas,,,i] <-
                catch(immNumRec[commAreas,,,i],
                      i,
                      opt$Fycomm,
                      opt$salphacomm,
                      opt$sbetacomm,
                      opt$numoftimesteps,
                      opt$numobs,
                      opt$lt)
            }
            if('mat' %in% opt$comm.catches){
              matCcomm[commAreas,,,i] <-
                catch(matNumRec[commAreas,,,i],
                      i,
                      opt$Fycomm,
                      opt$salphacomm,
                      opt$sbetacomm,
                      opt$numoftimesteps,
                      opt$numobs,
                      opt$lt)
            }
          }
      }
      if(num %in% opt$survstep) 
        {
          if(sum(opt$doescatchsurv) > 0)
            {
              if('imm' %in% opt$surv.catches){
                immCsurv[surveyAreas,,,i] <-
                  catch(immNumRec[surveyAreas,,,i],
                        i,
                        opt$Fysurv,
                        opt$salphasurv,
                        opt$sbetasurv,
                        1,
                        opt$numobs,
                        opt$lt)
              }
              if('mat' %in% opt$surv.catches){
                matCsurv[surveyAreas,,,i] <-
                  catch(matNumRec[surveyAreas,,,i],
                        i,
                        opt$Fysurv,
                        opt$salphasurv,
                        opt$sbetasurv,
                        1,
                        opt$numobs,
                        opt$lt)
              }
            }
        }

  #############
  # Overconsumption check
      tempimmC<-adjustconsumption(C=immCcomm[,,,i],
                                  S=immCsurv[,,,i],
                                  E=Eat[,,,i],
                                  N=immNumRec[,,,i],
                                  opt$maxratioconsumed,
                                  opt$numofareas)
      tempmatC<-adjustconsumption(C=matCcomm[,,,i],
                                  S=matCsurv[,,,i],
                                  ,
                                  N=matNumRec[,,,i],
                                  opt$maxratioconsumed,
                                  opt$numofareas)

  #############
  # Subtract Consumption from stock
      if(opt$doeseat==1)
        {
          immNumRec[,,,i] <- immNumRec[,,,i] - tempimmC$E[,,]
        }

  ##########
  # Subtract Catch from stock
      if(num==opt$survstep) # only have survey in one timestep of the year
        {
          surveyAreas <- 1:opt$numofareas %in% opt$doescatchsurv
          immNumRec[surveyAreas,,,i] <- immNumRec[surveyAreas,,,i]-
            tempimmC$S[surveyAreas,,]
          matNumRec[surveyAreas,,,i] <- matNumRec[surveyAreas,,,i]-
            tempmatC$S[surveyAreas,,]
        }
      immNumRec[commAreas,,,i] <- immNumRec[commAreas,,,i]-
        tempimmC$C[commAreas,,]
      matNumRec[commAreas,,,i] <- matNumRec[commAreas,,,i]-
        tempmatC$C[commAreas,,]



  ###########
  # Length update and natural mortality
      for(area in 1:opt$numofarea){
        immNumRec[area,,,i] <- t(G)%*%immNumRec[area,,,i]%*%immMort
        matNumRec[area,,,i] <- t(G)%*%matNumRec[area,,,i]%*%matMort
      }
  ###########
  # Recruits
      if(opt$doesmove!=1){
        matNumRec[,,1,i]<-matNumRec[,,1,i]+Rec[,,i]
      }
      immNumRec[,,1,i]<-immNumRec[,,1,i]+Rec[,,i]
    }

  
  sim <- list(Rec=Rec,
              immStart=immStart,
              matStart=matStart,
              immNumRec=immNumRec,
              matNumRec=matNumRec,
              immCsurv=immCsurv,
              matCsurv=matCsurv,
              immCcomm=immCcomm,
              matCcomm=matCcomm,
              Eat=Eat,
              GrowthProb=G,
              immMort=immMort,
              matMort=matMort,
              opt=opt)
  return(sim)
}
