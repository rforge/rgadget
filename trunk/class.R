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
         #package="rgadget"
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
           lt = "numeric",
           lengthgrouplen = "integer",
           numoflgroups = "integer",
           weight  = "numeric",
           mu = "numeric",
           sigma = "numeric",
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
           Eat = "array",
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
            ## other setup
            opt,
            probarea = 1,
            minage = 1,
            maxage = 10,
            minlength = 5,
            maxlength = 90,
            lengthgrouplen = 1,
            ## migration
            doesmigrate = 1,
            migrationP = array(c(1,0.6,0.6,1,0.6,1,1,0.6),c(4,2)),
            doesfuncmigrate = 0,
            diffusion = 0,
            driftx = 0,
            lambda = 0,
            ## recruitment
            doesmove = 0,
            doesrenew = 1,
            ## growth parameters
            doesgrow = 1,
            growthfunction = 'lengthvbsimple',
            lsup = 115,
            k = 0.09,
            binn = 15,
            beta = 200,
            recruitment.length = NULL,
            sigma = c(2.2472, 2.8982, 4.0705, 4.9276,
              5.5404, 5.8072, 6.0233, 8, 9, 9),
            ## consumption
            doeseat = 0,
            stocks.eaten = '',
            s.alpha = 0,
            s.beta = 0,
            
            ## maximum consumption
            m0 = 10^(-2),
            m3 = 3,
            H = 4000,
            maxratioconsumed = 0.95,
            ## weight parameters
            a = 10^(-5),
            b = 3,
            ## natural mortality
            z=0.2
            
            ){
            .Object@name <- name
            .Object@livesonareas <- livesonareas
            if(length(probarea) < length(livesonareas)){
              warning('length(probarea) < length(livesonareas) - even distribution assumed')
              probarea <- rep(1,length(livesonareas))/length(livesonareas)
              }
            .Object@probarea <- probarea
            .Object@minage <- as.integer(minage)
            .Object@maxage <- as.integer(maxage)
            .Object@minlength <- as.integer(minlength)
            .Object@maxlength <- as.integer(maxlength)
            .Object@lengthgrouplen <- as.integer(lengthgrouplen)
            .Object@numoflgroups <- length(.Object@l)
            .Object@doesmigrate <- as.integer(doesmigrate)
            .Object@doesfuncmigrate <- as.integer(doesfuncmigrate)
            .Object@diffusion <- diffusion
            .Object@driftx <- driftx
            .Object@lambda <- lambda
            .Object@migrationP <- migrationP
            .Object@doesmove <- as.integer(doesmove)
            .Object@doesrenew <- as.integer(doesrenew)
            .Object@doesgrow <- as.integer(doesgrow)
            .Object@growthfunction <- growthfunction
            .Object@doeseat <- as.integer(doeseat)
            tmp <- data.frame(stocks=stocks.eaten,
                              s.alpha=s.alpha,
                              s.beta=s.beta,
                              suitability='exponential')
            .Object@suitability <- tmp
            .Object@z <- z # c(rep(z,maxage-1),0.5)
            .Object@l <-  as.integer(seq(minlength,maxlength,lengthgrouplen))
            .Object@lt <- (.Object@l[2:length(.Object@l)]+
                           .Object@l[1:(length(.Object@l)-1)])/2
            .Object@numoflgroups <- as.integer(length(.Object@l) - 1)
            .Object@maxConsumption <- m0*.Object@lt^m3*12*opt@dt
            .Object@maxrationconsumed <- maxratioconsumed
            .Object@weight <- a*.Object@lt^b
            ## mu[i] mean length at age i
            .Object@mu <- lsup*(1-exp(-k*1:maxage))
            if(!is.null(recruitment.length))
              .Object@mu[1] <- recruitment.length
            .Object@sigma <- sigma
            G <- growthprob(.Object@lt,
                            beta,
                            lsup,
                            k,
                            opt@dt,
                            .Object@lengthgrouplen,
                            binn)
            ## stock arrays
            

            .Object@stock <- array(0,c(opt@numofareas,
                                       .Object@numoflgroups,
                                       maxage - minage +1,
                                       (opt@numobs*opt@numoftimesteps)))
            dimnames(.Object@stock) <-
              list(area=opt@areas,
                   length=.Object@l[-1],
                   age=minage:maxage,
                   time=paste(sprintf('Year_%s',rep(1:opt@numobs,
                     each=opt@numoftimesteps)
                     ),
                     sprintf('Step_%s',rep(1:opt@numoftimesteps,
                                           opt@numobs)),
                     sep='_'))
            .Object@consumed <- array(0,c(opt@numofareas,
                                          .Object@numoflgroups,
                                          maxage - minage +1,
                                          (opt@numobs*opt@numoftimesteps),
                                          opt@num.of.fleets))
            dimnames(.Object@consumed) <-
              list(area=opt@areas,
                   length=.Object@l[-1],
                   age=minage:maxage,
                   time=paste(sprintf('Year_%s',rep(1:opt@numobs,
                     each=opt@numoftimesteps)
                     ),
                     sprintf('Step_%s',rep(1:opt@numoftimesteps,
                                           opt@numobs)),
                     sep='_'),
                   fleet=1:opt@num.of.fleets)
            .Object@Eat <- .Object@stock
            return(.Object)
          }
          
          )

setGeneric('Init.pop',
           function(.Object,n){standardGeneric('Init.pop')}
           )

setMethod(f = "Init.pop",
          signature = "Stock",
          definition = function(.Object,
            n){
            start <- firststep(n,
                               .Object@mu,
                               .Object@sigma,
                               .Object@l,
                               .Object@z,
                               length(.Object@livesonareas),
                               .Object@probarea,
                               .Object@minage,
                               .Object@maxage
                      )
            if(.Object@doesmove == 1){
              .Object@stock[.Object@livesonareas,,-1,1] <-
                start[,,(.Object@minage+1):.Object@maxage]
            } else { 
              .Object@stock[.Object@livesonareas,,,1] <-
                start[,,.Object@minage:.Object@maxage]
            }
            return(.Object)
          }
          )


setGeneric('as.data.frame.Stock',
           function(.Object,n){standardGeneric('as.data.frame.Stock')}
           )
setMethod("as.data.frame.Stock",
          signature = "Stock",
          function(.Object){
            stock.table <- as.data.frame.table(.Object@stock,
                                               stringsAsFactors=FALSE)
            names(stock.table)[length(names(stock.table))] <- 'Num.indiv'
            catch <- as.data.frame.table(.Object@consumed,
                                         stringsAsFactors=FALSE)
            names(catch)[length(names(catch))] <- 'Catch'
            stock.table <- merge(stock.table,catch,all=TRUE)
            stock.table$year <- sapply(strsplit(stock.table$time,'_'),function(x) as.numeric(x[2]))
            stock.table$step <- sapply(strsplit(stock.table$time,'_'),function(x) as.numeric(x[4]))
            stock.table$length <- as.numeric(stock.table$length)
            stock.table$age <- as.numeric(stock.table$age)
            stock.table$weight <- .Object@weight[stock.table$length]  
            return(stock.table)
          }
)
##### test
opt <- new('Gadget.setup','Iceland',1:2,100,10,4,5,8000,2,2)
surv <- new('Fleet','survey',2, '', 0.1, 1:2, c('cod','haddock'), 0.1, 0.1)
imm <- new('Stock','cod',1:2,opt) 
imm <- Init.pop(imm,1e6)

#blu <- as.data.frame(imm)
#rest <- blu$step==1&blu$year==1
#tmp <- aggregate(Num.indiv~length + area,blu[rest,],sum)
#xyplot(Num.indiv~length|area,blu,type='l',
#       panel=function(x,y,...){
#         tmp <- aggregate(y,by=list(x),sum)
#         names(tmp) <- c('x','y')
#         panel.xyplot(tmp$x,tmp$y,...)
#       }
#        ) 
