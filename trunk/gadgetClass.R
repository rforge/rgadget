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
                        growthfunction = 'character',
                        growthparameters = 'list',
                        growthimplementation = 'list',
                        naturalmortality = 'numeric',
                        ## consumption
                        iseaten = 'numeric',
                        preylengths = 'data.frame',
                        energycontent = 'numeric',
                        doeseat = 'numeric',
                        suitability = 'data.frame',
                        preference = 'data.frame',
                        maxconsumption = 'numeric',
                        halffeedingvalue = 'numeric',
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

setMethod('initialize','gadget-stock',
          function(.Ob, file='stock'){
              stock <- strip.comments(sf)
              st.names <- sapply(stock[1:9],function(x) x[1])
              st <- sapply(stock[1:9],function(x) x[-1]) 
              names(st) <- st.names

              ## put stuff into the 
              .Ob@stockname <- st[['stockname']]
              .Ob@livesonares <- as.numeric(st['livesonares'])
              .Ob@minage <- as.numeric(st['minage'])
              .Ob@maxage <- as.numeric(st['maxage'])
              .Ob@minlength <- as.numeric(st['minlength'])
              .Ob@maxlength <- as.numeric(st['maxlength'])
              .Ob@dl <- as.numeric(st['dl'])

              .Ob@refweight <- read.table(st[['refweightfile']])
              names(.Ob@refweight) <- c('length','weight')

              .Ob@growthandeatlengths <- read.table(st[['growthandeatlengths']])
              names(.Ob@growthandeatlengths) <-
                c('lengthgroup','min','max')
              
              ## pop from list
              stock[1:9] <- NULL
              
              ## check 'doesgrow switch
              if(stock[[1]][2]==0){
                .Ob@doesgrow <- 0
                .Ob@growthfunction <- NULL
                stock[1] <- NULL
              } else{
                .Ob@doesgrow <- 1
                .Ob@growthfunction <- stock[[2]][2]               
                if(stock[[2]][2] == 'weightjones'){
                  .Ob@growthparameters <- 
                    list(wgrowthparameters = stock[[3]][-1],
                         lgrowthparameters = stock[[4]][-1])
                  stock[1:4] <- NULL
                } else if(stock[[2]][2] == 'weightvbexpanded'){
                  .Ob@growthparameters <- 
                    list(wgrowthparameters = stock[[3]][-1],
                         lgrowthparameters = stock[[4]][-1],
                         yeareffect = stock[[5]][-1],
                         stepeffect = stock[[6]][-1],
                         areaeffect = stock[[7]][-1])
                  stock[1:7] <- NULL
                } else if(stock[[2]][2] %in% c('lengthvb','lengthpower')){
                  .Ob@growthparameters <-                     
                    list(growthparameters = stock[[3]][-1],
                         weightparameters = stock[[4]][-1])
                  stock[1:4] <- NULL
                } else {
                  .Ob@growthparameters <-
                    list(growthparameters = stock[[3]][-1])
                  stock[1:3] <- NULL
                }
              }
              implementation <- lapply(stock[1:2],function(x) x[-1])
              names(implementation) <- lapply(stock[1:2],function(x) x[1])
              stock[1:2] <- NULL
              
              .Ob@growthimplementation <- implementation
              .Ob@naturalmortality <- stock[[1]][-1]

              stock[1] <- NULL
              
              ## iseaten
              if(stock[[1]][2]==0){
                .Ob@iseaten <- 0
                stock[1] <- NULL
              } else {
                .Ob@iseaten <- 1
                .Ob@preylengths <- read.table(stock[[2]][2])
                names(.Ob@preylengths) <- 
                  c('lengthgroup','min','max')
                .Ob@energycontent <- as.numeric(stock[[3]][2])
                stock[1:3] <- NULL
              }

    ## doeseat
              if(stock[[1]][2]==0){
                .Ob@doeseat <- 0
                stock[1] <- NULL
              } else {
                stock[1] <- NULL
                pref <- grep('preference',stock)
                suit <- grep('suitability',stock)
                maxcon <- grep('maxconsumption',stock)
                
                suitability <- lapply(stock[2:(pref-1)],function(x) x[-1])
      names(suitability) <- sapply(stock[2:(pref-1)],function(x) x[1])

      preference <- t(sapply(stock[pref:(maxcon-1)],function(x) x))
      names(preference) <- c('preyname','preference')
      pred.info <-
        list(suitability=suitability,
             preference=preference,
             maxconsumption=stock[[maxcon]][-1])
      stock[1:maxcon] <- NULL
    }

    
    
    
  }
  stocks <- within(list(),
                   for(sf in stock.files){
                     tmp.func(sf)
                   })
  stocks$sf <- NULL
          }
