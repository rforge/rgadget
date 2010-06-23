setClass(
         Class = "Gadget.setup",
         representation = representation(
           name = "character",
           areas = "integer",
           areasize = "numeric",
           numobs = "integer",
           numoftimesteps = "integer",
           current.time.step = "integer",
           dt = "numeric",
           numofareas = "integer",
           area.temperature = "numeric",
           otherfood = "array",
           num.of.stocks = "integer",
           num.of.fleets = "integer" 
           )
         
         )
setMethod(f = "initialize",
          signature = "Gadget.setup",
          definition = function(.Object, name, areas, areasize,
            numobs, numoftimesteps, 
            area.temperature, otherfood,num.of.stocks, num.of.fleets){
            .Object@name <-  name
            .Object@areas <- as.integer(areas)
            .Object@numofareas <- as.integer(length(areas))
            .Object@numobs <- as.integer(numobs)
            .Object@numoftimesteps <- as.integer(numoftimesteps)
            .Object@area.temperature <- area.temperature
            .Object@current.time.step <- as.integer(0)
            .Object@otherfood <-
              array(rep(otherfood,
                        numobs*numoftimesteps*.Object@numofareas),
                    c(.Object@numofareas,numobs*numoftimesteps))
            .Object@num.of.stocks <- as.integer(num.of.stocks)
            .Object@num.of.fleets <- as.integer(num.of.fleets)
            .Object@dt <- 1/numoftimesteps
            return(.Object)
          }
          
          )


setClass(
         Class = "Fleet",
         representation = representation(
           name = "character",  
           timesteps = "integer",
           suitability = "data.frame",
           fleettype = "character",
           fy = "numeric",
           livesonareas = 'numeric'
           ),
         #package="rgadget"
         )

setMethod(
          f = "initialize",
          signature = "Fleet",
          definition = function(.Object,
            name,
            timesteps,
            fleettype,
            fy,
            livesonareas,
            stocks.caught,
            alpha,
            beta){
            .Object@name <- name
            .Object@timesteps <- as.integer(timesteps)
            .Object@fleettype <- fleettype
            .Object@fy <- fy
            .Object@livesonareas <- as.integer(livesonareas)
            tmp <- data.frame(stocks=stocks.caught,
                              alpha=alpha,
                              beta=beta,
                              suitability='exponential')
            .Object@suitability <- tmp
            return(.Object)
          }
          )

setClass(
         Class = 'Stock',
         representation=representation(
           name = "character",
           livesonareas = "integer",
           probarea = "numeric",
           minage = "integer",
           maxage = "integer",
           minlength = "integer",
           maxlength = "integer",
           l = "integer",
           lengthgrouplen = "integer",
           ## migration
           doesmigrate = "integer",
           migrationP = "array",
           doesfuncmigrate = "integer",
           diffusion = "numeric",
           driftx = "numeric",
           lambda = "numeric",
           ## recruitment
           doesmove = "integer",
           doesrenew = "integer",
           ## growth parameters
           doesgrow = "integer",
           growthfunction = "character",
           #lsup = "numeric",
           #k = "numeric",
           #binn = "integer",
           #beta = "numeric",
           G = "array",
           ## consumption
           doeseat = "integer",
           suitability = "data.frame",
           ## maximum consumption
           #m0 = "numeric",
           #m3 = "numeric",
           #H = "numeric",
           maxrationconsumed = "numeric",
           maxConsumption = "numeric",
           ## number of indivuals consumed and alive
           consumed = "array",
           stock = "array",
           ## natural mortality
           z="numeric"
           ),
                                        #package="rgadget"
         )

setMethod(
          f = "initialize",
          signature = "Stock",
          definition = function(.Object,
            name,
            livesonareas,
            probarea,
            minage,
            maxage,
            minlength,
            maxlength,
            dl,
            ## migration
            doesmigrate,
            migrationP,
            doesfuncmigrate = 0,
            diffusion = NULL,
            driftx = NULL,
            lambda = NULL,
            ## recruitment
            doesmove,
            doesrenew,
            ## growth parameters
            doesgrow,
            growthfunction,
            lsup,
            k,
            binn,
            beta,
            recruitment.length = NULL,
            ## consumption
            doeseat,
            stocks.eaten,
            s.alpha,
            s.beta,
            
            ## maximum consumption
            m0,
            m3,
            H,
            maxrationconsumed,
            ## weight parameters
            a,
            b,
            ## number of indivuals consumed and alive
            consumed,
            stock,
            ## natural mortality
            z,
            ## other setup
            opt
            ){
            .Object@name <- name
            .Object@livesonareas <- livesonareas
            .Object@probarea <- probarea
            .Object@minage <- minage
            .Object@maxage <- maxage
            .Object@minlength <- minlength
            .Object@maxlength <- maxlength
            .Object@lengthgrouplen <- lengthgrouplen
            .Object@doesmigrate <- doesmigrate
            .Object@doesfuncmigrate <- doesfuncmigrate
            .Object@diffusion <- diffusion
            .Object@driftx <- driftx
            .Object@lambda <- lambda
            .Object@migrationR <- migrationR
            .Object@doesmove <- doesmove
            .Object@doesrenew <- doesrenew
            .Object@doesgrow <- doesgrow
            .Object@growthfunction <- growthfunction
            .Object@doeseat <- doeseat
            tmp <- data.frame(stocks=stocks.eaten,
                              s.alpha=alpha,
                              s.beta=beta,
                              suitability='exponential')
            .Object@suitability <- tmp
            .Object@z <- c(rep(z,maxage-1),0.5)
            .Object@l <-  seq(minlen,maxlen,lengthgrouplen)
            .Object@maxConsumption <- m0*lt^m3*12*opt@dt
            .Object@maxrationconsumed <- maxratioconsumed
            .Object@weight <- a*.Object@lt^b
            ## mu[i] mean length at age i
            .Object@mu <- lsup*(1-exp(-k*1:maxage))
            if(!is.null(recruitment.length))
              .Object@mu[1] <- recruitment.length
            G <- growthprob(.Object@lt,
                            beta,
                            lsup,
                            k,
                            opt@dt,
                            .Object@lengthgrouplen,
                            binn)
            ## stock arrays
            

            .Object@stock <- array(0,c(opt@numofareas,
                         numoflgroups,
                         maxage - minage +1,
                         (opt@numobs*opt@numoftimesteps)))
            dimnames(.Object@stock) <-
              list(area=opt@areas,
                   length=minlen:(maxlen-1),
                   age=minage:maxage,
                   time=paste(sprintf('Year_%s',rep(1:opt@numobs,
                     each=opt@numoftimesteps)
                     ),
                     sprintf('Step_%s',rep(1:opt@numoftimesteps,
                                           opt@numobs)),
                     sep='_'))
            .Object@consumed <- array(0,c(opt@numofareas,
                                          numoflgroups,
                                          maxage - minage +1,
                                          (opt@numobs*opt@numoftimesteps),
                                          opt@num.of.fleets))
            dimnames(.Object@consumed) <-
              list(area=opt@areas,
                   length=minlen:(maxlen-1),
                   age=minage:maxage,
                   time=paste(sprintf('Year_%s',rep(1:opt@numobs,
                     each=opt@numoftimesteps)
                     ),
                     sprintf('Step_%s',rep(1:opt@numoftimesteps,
                                           opt@numobs)),
                     sep='_'),
                   fleet=1:opt@num.of.fleets)
            return(.Object)
          }
          
          )



##### test
opt <-new('Gadget.setup','Iceland',1:10,100,10,4,5,8000,2,2)
surv <- new('Fleet','survey',2, '', 0.1, 2:5, c('cod','haddock'), 0.1, 0.1)
