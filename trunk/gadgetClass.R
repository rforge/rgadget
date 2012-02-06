setClass('gadget-stock',
         representation(stockname = 'character',
                        ## setup
                        livesonareas = 'numeric',
                        minage = 'numeric',
                        maxage = 'numeric',
                        minlength = 'numeric',
                        maxlength = 'numeric',
                        dl = 'numeric',
                        refweight  = 'data.frame',
                        growthandeatlengths = 'data.frame',
                        ## growth
                        doesgrow = 'numeric',
                        growth = 'gadget-growth'
                        naturalmortality = 'numeric',
                        ## consumption
                        iseaten = 'numeric',
                        preyinfo = 'gadget-prey',
                        doeseat = 'numeric',
                        predator = 'gadget-predator'
                        ## init stuff
                        initialconditions = 'list',
                        initialdata = 'data.frame',
                        ## migration
                        doesmigrate = 'numeric',
                        yearstep = 'data.frame',
                        migrationmatrix = 'array',
                        migrationratio = 'data.frame',
                        ## Maturation
                        doesmature = 'numeric',
                        maturityfunction = 'character',
                        maturestocksandratios = 'list',
                        coefficients = 'list',
                        maturitysteps = 'numeric',
                        maturitylengths = 'numeric',
                        ## movement between stocks
                        doesmove = 'numeric',
                        transitionstockandratios = 'list',
                        transitionstep = 'numeric',
                        ## renewal
                        doesrenew = 'numeric',
                        renewal = 'list',
                        renewal.data = 'data.frame',
                        ## spawning -- to be implemented
                        doesspawn = 'numeric',
                        ## straying -- to be implemented
                        doesstray = 'numeric'
                        )
         )

setClass('gadget-growth',
         representation(growthfunction = 'character',
                        growthimplementation = 'character',
                        ## growthfunction parameters
                        growthparameters = 'vector',
                        wgrowthparameters = 'vector',
                        lgrowthparameters = 'vector',
                        weigthgrowthdata = 'dataframe',
                        yeareffect = 'vector',
                        stepeffect = 'vector',
                        areaeffect = 'vector',
                        ## growth implementation
                        beta = 'vector',
                        maxlengthgroupgrowth = 'vector'                        
                        ))

setClass('gadget-prey',
         representation(preylengths = 'dataframe',
                        engergycontent = 'numeric'))

setClass('gadget-predator',
         representation(suitability = 'list',
                        preference = 'data.frame',
                        maxconsumption = 'numeric',
                        halffeedingvalue = 'numeric'))

setMethod('initialize',
          'gadget-stock',
          function(.Object, file='stock'){
              stock <- strip.comments(file)
              st.names <- sapply(stock[1:9],function(x) x[1])
              st <- sapply(stock[1:9],function(x) x[-1]) 
              names(st) <- st.names

              ## Basic properties
              .Object@stockname <- st[['stockname']]
              .Object@livesonareas <- as.numeric(st['livesonareas'])
              .Object@minage <- as.numeric(st['minage'])
              .Object@maxage <- as.numeric(st['maxage'])
              .Object@minlength <- as.numeric(st['minlength'])
              .Object@maxlength <- as.numeric(st['maxlength'])
              .Object@dl <- as.numeric(st['dl'])

              .Object@refweight <- read.table(st[['refweightfile']],
                                              comment.char=';')
              names(.Object@refweight) <- c('length','weight')

              .Object@growthandeatlengths <-
                read.table(st[['growthandeatlengths']],
                           comment.char=';')
              names(.Object@growthandeatlengths) <-
                c('lengthgroup','min','max')
              
              ## pop from list
              stock[1:9] <- NULL
              
              ## check 'doesgrow switch
              if(stock[[1]][2]==0){
                .Object@doesgrow <- 0
                .Object@growthfunction <- NULL
                stock[1] <- NULL
              } else{
                ## create a gadget-growth object
                growthparameters <- NULL
                wgrowthparameters <- NULL
                lgrowthparameters <- NULL
                yeareffect <- NULL
                stepeffect <- NULL
                areaeffect <- NULL
                weightgrowthdata <- NULL
                
                .Object@doesgrow <- 1
                growthfunction <- stock[[2]][2]               
                if(stock[[2]][2] == 'weightjones'){
                  wgrowthparameters <- merge.formula(stock[[3]][-1])
                  lgrowthparameters <- merge.formula(stock[[4]][-1])
                  stock[1:4] <- NULL
                } else if(stock[[2]][2] == 'weightvbexpanded'){                
                  wgrowthparameters <- merge.formula(stock[[3]][-1])
                  lgrowthparameters <- merge.formula(stock[[4]][-1])
                  yeareffect <- merge.formula(stock[[5]][-1])
                  stepeffect <- merge.formula(stock[[6]][-1])
                  areaeffect <- merge.formula(stock[[7]][-1])
                  stock[1:7] <- NULL
                } else if(stock[[2]][2] %in% c('lengthvb','lengthpower')){
                  growthparameters <- merge.formula(stock[[3]][-1])
                  weightgrowthdata <- read.table(stock[[4]][-1],
                                                 comment.char = ';')
                  stock[1:4] <- NULL
                } else {
                  growthparameters <- merge.formula(stock[[3]][-1])
                  stock[1:3] <- NULL
                }
              }
              .Object@growth <- new('gadget-growth',
                                    growthparameters = growthparameters,
                                    lgrowthparameters = lgrowthparameters,
                                    wgrowthparameters = wgrowthparameters,
                                    yeareffect = yeareffect,
                                    stepeffect = stepeffect,
                                    areaeffect = areaeffect,
                                    beta = merge.formula(stock[[1]][-1]),
                                    maxlengthgroupgrowth =
                                    merge.formula(stock[[2]][-1]))
              stock[1:2] <- NULL
              
              .Object@naturalmortality <- as.numeric(stock[[1]][-1])

              stock[1] <- NULL
              
              ## iseaten
              if(stock[[1]][2]==0){
                .Object@iseaten <- 0
                stock[1] <- NULL
              } else {
                .Object@iseaten <- 1
                preylengths <- read.table(stock[[2]][2],
                                          comment.char=';')                     
                names(.Object@preylengths) <- c('lengthgroup','min','max')
                .Object@preyinfo <-
                  new('gadget-prey',
                      preylength = preylengths,
                      energycontent = as.numeric(stock[[3]][2]))
                stock[1:2] <- NULL
              }

              ## doeseat
              if(stock[[1]][2]==0){
                .Object@doeseat <- 0
                stock[1] <- NULL
              } else {
                stock[1] <- NULL
                pref <- grep('preference',stock)
                suit <- grep('suitability',stock)
                maxcon <- grep('maxconsumption',stock)
                ## read suitability parameters
                suitability <-
                  lapply(stock[2:(pref-1)],function(x) merge.formula(x[-1]))
                names(suitability) <-
                  sapply(stock[2:(pref-1)],function(x) x[1])

                ## -- prey preference
                preference <-
                  t(sapply(stock[pref:(maxcon-1)],function(x) x))
                names(preference) <- c('preyname','preference')
                new('gadget-predator',
                    suitability = suitability,
                    preference = preference,
                    maxconsumption = merge.formula(stock[[maxcon]][-1]),
                    halffeedingvalue = merge.formula(stock[[maxcon+1]][-1]))
                stock[1:(maxcon+1)] <- NULL
              }
              ## initial conditions
              
              
              return(.Object)
            }
)    
    

