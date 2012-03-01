setClass('gadget-growth',
         representation(growthfunction = 'character',
                        growthimplementation = 'character',
                        ## growthfunction parameters
                        growthparameters = 'vector',
                        wgrowthparameters = 'vector',
                        lgrowthparameters = 'vector',
                        weigthgrowthdata = 'data.frame',
                        yeareffect = 'vector',
                        stepeffect = 'vector',
                        areaeffect = 'vector',
                        ## growth implementation
                        beta = 'vector',
                        maxlengthgroupgrowth = 'vector'                        
                        ),
         prototype(growthfunction = '',
                        growthimplementation = 'betabinomial',
                        ## growthfunction parameters
                        growthparameters = vector(),
                        wgrowthparameters = vector(),
                        lgrowthparameters = vector(),
                        weigthgrowthdata = data.frame(),
                        yeareffect = vector(),
                        stepeffect = vector(),
                        areaeffect = vector(),
                        ## growth implementation
                        beta = vector(),
                        maxlengthgroupgrowth = vector()                        
         ),
         #package = 'rgadget',
         validity = function(x){
           if(x@growthfunction == 'lengthvbsimple')
             if(length(x@growthparameters)!=5)
               stop('Growth-parameters misspecified for lengthvbsimple
                    should be 5')
           else 
             return(TRUE)
         }
         )

setClass('gadget-prey',
         representation(preylengths = 'data.frame',
                        engergycontent = 'numeric'))
         ##package = 'rgadget')

setClass('gadget-predator',
         representation(suitability = 'list',
                        preference = 'data.frame',
                        maxconsumption = 'numeric',
                        halffeedingvalue = 'numeric'))
         ##package = 'rgadget')

setClass('gadget-fleet',
         representation(name = 'character',
                        type = 'character',
                        livesonareas = 'numeric',
                        multiplicative = 'numeric',
                        suitability = 'list',
                        catchability = 'data.frame',
                        quotafunction = 'character',
                        biomasslevel = 'numeric',
                        quotalevel = 'numeric',
                        amount = 'data.frame'),
         prototype(name = 'fleet',
                 type = '',
                 multiplicative = 1,
                 suitability = list(),
                 catchability = data.frame(),
                 quotafunction = '',
                 biomasslevel = 0,
                 amount = data.frame()),
         ##package = 'rgadget'
         )

setClass('gadget-time',
         representation(firstyear = 'numeric', ## first year of simulation
                        firststep = 'numeric', ## first time step within first
                                               ## year of simulation.
                        lastyear = 'numeric', ## last year of simulation
                        laststep = 'numeric', ## last step of last year
                        notimesteps = 'numeric'), ## vector of lengths of timeintervals
         ###package = 'rgadget',
         validity = function(x){
           if(x@firstyear > x@lastyear)
             stop('Firstyear after lastyear')
           if(sum(x@notimesteps) != 12)
             stop('notimesteps should sum up to 12')
           if(!(x@firststep %in% seq(along = x@notimesteps)) )
             stop('firststep not in the range of timesteps')
           if(!(x@laststep %in% seq(along = x@notimesteps)) )
             stop('laststep not in the range of timesteps')
           return(TRUE)
         }
      ) 

setClass('gadget-area',
         representation(area = 'numeric', ## vector of area identifiers
                        size = 'numeric', ## vector of area sizes
                        temperature = 'data.frame'),
         ###package = 'rgadget',
         validity = function(x){
           if(x@area < 1)
             stop('illegal area')
           if(x@size < 0)
             stop('negative size')
           return(TRUE)
         }
         )
setMethod('initialize','gadget-area',
          function(.Object,area,size,temperature){
            .Object@area <- area
            .Object@size <- size
            .Object@temperature <- temperature[c('year','step','area','temperature')]
            return(.Object)
          })

setClass('gadget-otherfood',
         representation(foodname = 'character',
                        livesonaareas = 'character',
                        lengths = 'numeric',
                        energycontent = 'numeric',
                        amount = 'data.frame'))
         ##package = 'rgadget')


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
                        growth = 'gadget-growth',
                        naturalmortality = 'numeric',
                        ## consumption
                        iseaten = 'numeric',
                        preyinfo = 'gadget-prey',
                        doeseat = 'numeric',
                        predator = 'gadget-predator',
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
                        ## spawning 
                        doesspawn = 'numeric',
                        spawning = new('gadget-spawning'),
                        ## straying -- to be implemented
                        doesstray = 'numeric'
                        ),
         prototype(stockname = '',
                        ## setup
                        livesonareas = NULL,
                        minage = NULL,
                        maxage = NULL,
                        minlength = NULL,
                        maxlength = NULL,
                        dl = NULL,
                        refweight  = data.frame(),
                        growthandeatlengths = data.frame(),
                        ## growth
                        doesgrow = 0,
                        growth = new('gadget-growth'),
                        naturalmortality = NULL,
                        ## consumption
                        iseaten = 0,
                        preyinfo = new('gadget-prey'),
                        doeseat = 0,
                        predator = new('gadget-predator'),
                        ## init stuff
                        initialconditions = list(),
                        initialdata = data.frame(),
                        ## migration
                        doesmigrate = 0,
                        yearstep = data.frame(),
                        migrationmatrix = array(),
                        migrationratio = data.frame(),
                        ## Maturation
                        doesmature = 0,
                        maturityfunction = '',
                        maturestocksandratios = list(),
                        coefficients = list(),
                        maturitysteps = 0,
                        maturitylengths = 0,
                        ## movement between stocks
                        doesmove = 0,
                        transitionstockandratios = list(),
                        transitionstep = 0,
                        ## renewal
                        doesrenew = 0,
                        renewal = list(),
                        renewal.data = data.frame(),
                        ## spawning
                        doesspawn = 0,
                        spawning = new('gadget-spawning'),
                        ## straying -- to be implemented
                        doesstray = 0
                   )
         ##package = 'rgadget'
         )

setClass('gadget-spawning',
         representation(
           spawnsteps = 'numeric',
           spawnareas = 'numeric',
           firstspawnyear = 'numeric',
           lastspawnyear = 'numeric',
           spawnstocksandratio = 'data.frame',
           proportionfunction = 'list',
           mortalityfunction = 'list',
           weightlossfunction = 'list',
           recruitment = 'list',
           stockparameters = 'data.frame'),         
        prototype(spawnsteps = 0,
                  spawnareas = 0,
                  firstspawnyear = 0,
                  lastspawnyear = 0,
                  spawnstocksandratio = data.frame(),
                  proportionfunction = list(func = 'constant', alpha = 1),
                  mortalityfunction = list(func = 'constant', alpha = 0),
                  weightlossfunction = list(func = 'constant', alpha = 0),
                  recruitment = list(func = 'simplessb', mu = 1),
                  stockparameters = data.frame(mean = NULL, sttdev = NULL, 
                                               alpha = NULL, beta = NULL))
           )

setClass('gadget-main',
         representation(model.name='character',
                        time = 'gadget-time',
                        area = 'gadget-area',
                        print = 'list',
                        stocks = 'list',
                        tags = 'list',
                        otherfood = 'list',
                        fleets = 'list',
                        likelihood = 'list'),
         ##package = 'rgadget'
         )

                        


read.gadget.stock <- function(file='stock'){
  .Object <- new('gadget-stock')
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
                                  comment.char=';',
                                  col.names = c('length','weight'))

  
  .Object@growthandeatlengths <-
    read.table(st[['growthandeatlengths']],
               comment.char = ';',
               col.names = c('lengthgroup','min','max'))
  
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

    

