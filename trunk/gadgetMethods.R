## Time file methods

setMethod("write",
    signature(x = "gadget-time"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
      header <- sprintf('; time file created in Rgadget\n; %s - %s',file,Sys.Date())
      time.file <-
        paste(header,
              paste('firstyear',x@firstyear,sep='\t'),
              paste('firststep',x@firststep,sep='\t'),
              paste('lastyear',x@lastyear,sep='\t'),
              paste('laststep',x@laststep,sep='\t'),
              paste('notimesteps',
                    paste(length(x@notimesteps),
                          paste(x@notimesteps,collapse=' ')),
                    sep='\t'),
              sep='\n')
      write(time.file,file=file)
    }
)
setGeneric('getTimeSteps',def=function(object){standardGeneric("getTimeSteps")})
setMethod('getTimeSteps','gadget-time',
          function(x) {
            year <- x@firstyear:x@lastyear
            data.frame(year = rep(year,each = length(x@notimesteps)),
                                 step = rep(seq(along = x@notimesteps), length(year)))
          })

## area-file methods

setMethod("write",
    signature(x = "gadget-area"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
      header <- sprintf('; time file created in Rgadget\n; %s - %s',file,Sys.Date())
      area.file <-
        paste(header,
              paste('areas',paste(x@areas,collapse=' '),sep='\t'),
              paste('size',paste(x@size,collapse=' '),sep='\t'),
              'temperature',
              '; year - step - area - temperature',
              sep='\n')
      write(area.file,file=file)
      write.table(x@temperature,file=file,col.names=FALSE,append=TRUE,
                  quote=FALSE,sep='\t',row.names=FALSE)
    }
)

## stockfile methods

setMethod("write",
    signature(x = "gadget-prey"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
      dir.create(sprintf('%s/Aggfiles',file), showWarnings = FALSE, recursive = TRUE)
      header <- paste(sprintf('; prey aggregation file for %s',x@name),
                      sprintf('; created using rgadget at %s', Sys.Date()),
                      sep = '\n')
      write(header,file = sprintf('%s/Aggfiles/%s.prey.agg',file,x@name))
      write.table(x@preylengths,file = sprintf('%s/Aggfiles/%s.prey.agg',file,x@name),
            col.names=FALSE,append=TRUE,
            quote=FALSE,sep='\t',row.names=FALSE)
      paste(sprintf('preylengths\tAggfiles/%s.prey.agg',x@name),
            sprintf('energycontent\t%s',x@energycontent),
            sep = '\n')    
    }
)

setMethod("write",
    signature(x = "gadget-stock"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        dir.create(sprintf('%s/Data', file), showWarnings = FALSE, recursive = TRUE)
        dir.create(sprintf('%s/Aggfiles', file), showWarnings = FALSE, recursive = TRUE)
        
        ref.head <- paste(sprintf('; refweight file for %s created using rgadget at %s',
                                  x@stockname,Sys.Date()),
                          paste(c('; ',names(x@refweight)),collapse = '\t'),
                          sep = '\n')
        write(ref.head,file = sprintf('%s/Data/%s.refweigthfile',file,x@stockname))
        tmp <- x@refweight
        tmp$weight <- round(tmp$weight)
        write.table(tmp,
                    file = sprintf('%s/Data/%s.refweigthfile',file,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)
        
        ## length aggregation
        lengths <- seq(300,900,by = 10)
        lenAgg <- data.frame(length = paste('len',tail(lengths,-1), sep = ''),
                             min = head(lengths,-1),
                             max = tail(lengths,-1)
                             )
        agg.head <- paste(sprintf('; length aggregation file for %s created using rgadget at %s',
                                  x@stockname,Sys.Date()),
                          paste(c('; ',names(lenAgg)),collapse = '\t'),
                          sep = '\n')
        write(agg.head,file = sprintf('%s/Aggfiles/%s.len.agg',file,x@stockname))
        write.table(lenAgg,
                    file = sprintf('%s/Aggfiles/%s.len.agg',file,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)
        
        ## initial data
        init.head <- paste(sprintf('; initial (normalcond) file for %s created using rgadget at %s',
                                  x@stockname,Sys.Date()),
                          paste(c('; ',names(x@initialdata)),collapse = '\t'),
                          sep = '\n')
        write(init.head,file = sprintf('%s/Data/%s.normalcond',file,x@stockname))
        write.table(x@initialdata,
                    file = sprintf('%s/Data/%s.normalcond',file,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)
        
        
        stock.text <- 
          c(sprintf('; stock definition file for %s created using rgadget',x@stockname),
            sprintf('; at %s',Sys.Date()),
            ';',
            sprintf('stockname\t%s',x@stockname),
            sprintf('livesonareas\t%s',paste(x@livesonareas,collapse = '\n')),
            sprintf('minage\t%s',x@minage),
            sprintf('maxage\t%s',x@maxage),
            sprintf('minlength\t%s',x@minlength),
            sprintf('maxlength\t%s',x@maxlength),
            sprintf('dl\t%s',x@dl),
            sprintf('refweightfile\tData/%s.refweigthfile',x@stockname),
            sprintf('growthandeatlengths\tAggfiles/%s.len.agg',x@stockname),
            sprintf('doesgrow\t%s',x@doesgrow),
            growth = ';',
            sprintf('naturalmortality\t%s',paste(x@naturalmortality,collapse = '\t')),
            sprintf('iseaten\t%s',x@iseaten),
            eaten = ';',
            sprintf('doeseat\t%s',x@doeseat),
            eat = ';',
            'Initialconditions',
            paste(c('minage', 'maxage', 'minlength',
                    'maxlength', 'dl', 'sdev'),
                  x@initialconditions,sep ='\t',collapse = '\n'),
            sprintf('normalcondfile\tData/%s.normalcond',x@stockname),
            sprintf('doesmigrate\t%s',x@doesmigrate),
            migration = ';',
            sprintf('doesmature\t%s',x@doesmature),
            maturity = ';',
            sprintf('doesmove\t%s',x@doesmove),
            movement = ';',
            sprintf('doesrenew\t%s',x@doesrenew),
            renewal = ';',
            sprintf('doesspawn\t%s',x@doesspawn),
            spawning = ';',
            sprintf('doesstray\t%s',x@doesstray),
            straying = ';')
        if(x@doesgrow == 1){
          stock.text['growth'] <- toString(x@growth)
        }
        if(x@iseaten == 1){
          stock.text['eaten'] <- write(x@preyinfo,file=file)
        }
        if(x@doeseat == 1){
          stock.text['eat'] <- toString(x@predator)
        }
        if(x@doesspawn){
          stock.text['spawning'] <- sprintf('spawnfile\tData/%s.spawnfile',x@stockname)
          write(x@spawning,file = sprintf('%s/Data/%s.spawnfile',file,x@stockname))
        }
        write(paste(stock.text,collapse = '\n'),
              file = sprintf('%s/%s',file,x@stockname))
    }
)

setMethod("toString",
          signature(x = "gadget-predator"),
          function (x, ...) 
          {
            tmp <- x@suitability
            tmp <- paste(tmp$stock,tmp$func,sapply(tmp$parameters,
                                            function(x) paste(x,collapse='\t')),
                         sep = '\t',collapse = '\n')
            pred.text <- 
              paste(sprintf('suitabiliy\n%s',tmp),
                    sprintf('preference\n%s',
                            paste(x@preference$stock,
                                  x@preference$preference,
                                  sep = '\t',
                                  collapse = '\n')),
                    sprintf('maxconsumption\t%s',
                            paste(x@maxconsumption,collapse = '\t')),
                    sprintf('halffeedingvalue\t%s',x@halffeedingvalue),
                    sep = '\n')
            return(pred.text)
          }
)

setMethod("toString",
          signature(x = "gadget-growth"),
          function (x, ...) 
          {
            growth.text <- 
              c(sprintf('growthfunction\t%s',x@growthfunction),
                params = '',
                sprintf('beta\t%s',x@beta),
                sprintf('maxlengthgroupgrowth\t%s',x@maxlengthgroupgrowth))
            if(x@growthfunction == 'lengthvbsimple')
              growth.text['params'] <- 
                paste(sprintf('growthparameters\t%s',
                              paste(x@growthparameters,collapse = '\t')))
            else 
              stop('other growth updates currently not supported by Rgadget')
            return(paste(growth.text,collapse = '\n'))
          }
          )



setMethod("write",
    signature(x = "gadget-fleet"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
      header <- sprintf('; fleet file created in Rgadget\n; %s - %s\n[fleetcomponent]',file,Sys.Date())
      ## default text
      fleet.text <- 
        c(header = header,
          name = sprintf('%s\t%s',x@type,x@name),
          area = sprintf('livesonareas\t%s',x@livesonareas),
          mulit = sprintf('multiplicative\t%s',x@multiplicative),
          suit = sprintf('suitability\n%s',
                  paste(x@suitability$preyname,
                        'function',
                        x@suitability$func,
                        paste(x@suitability$parameters,collapse = '\t'))),
          empty = '; empty space -- move along nothing to see here',
          amount = sprintf('amount\tData/%s.amount',x@name),
          ';')

      if(tolower(x@type) == 'quotafleet')
        fleet.text['empty'] <- 
          paste(sprintf('quotafunction\t%s',x@quotafunction),
                sprintf('biomasslevel\t%s',paste(x@biomasslevel,collapse = '\t')),
                sprintf('quotalevel\t%s',paste(x@quotalevel,collapse = '\t')),
                sep = '\n')
      else if(tolower(x@type) == 'effortfleet')
        fleet.text['empty'] <- 
          sprintf('catchability\n%s',
                  paste(paste(x@catchability$stock,
                              x@catchability$catchabilty,sep='\t'),collapse = '\n'))
      else 
        fleet.text <- fleet.text[names(fleet.text) != 'empty']
      
      write.table(x@amount,file=sprintf('%s/Data/%s.amount',file,x@name),
                  col.names=FALSE,
                  quote=FALSE,sep='\t',row.names=FALSE)
      if(file.exists(sprintf('%s/fleets',file))){
        write(paste(fleet.text,collapse='\n'),file=sprintf('%s/fleets',file),
              append = TRUE)
      } else {
        write(paste(fleet.text,collapse='\n'),file=sprintf('%s/fleets',file))
      }
      invisible(fleet.text)
    }
)


setMethod("write",
    signature(x = "gadget-main"),
    function (x, file = "gadget-models", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
      loc <- sprintf('%s/%s',file,x@model.name)
      dir.create(loc, showWarnings = FALSE, recursive = TRUE)
      ## writing ecosystem files
      write(x@area, file = sprintf('%s/area',loc))
      write(x@time, file = sprintf('%s/time',loc))
      if(length(x@print) > 0)
        write(x@print, file = sprintf('%s/printfile',loc))
      for(stock in x@stocks)
        write(stock,file = loc)
#      write(x@tags,file = sprintf('%s/tagfile',loc))
#      write(x@otherfood, file = sprintf('%s/otherfood',loc))
      file.remove(sprintf('%s/fleets',loc))
      for(fleet in x@fleets)
        write(fleet,file=loc)
      ## Likelihood files
      ##write(x@likelhood, file = sprintf('%s/likelihood'))
      
      main.text <-
        paste(sprintf('; main file for the %s model',x@model.name),
              sprintf('; created using rgadget at %s',Sys.Date()),
              'timefile\ttime',
              'areafile\tarea',
              sprintf('printfiles\t%s',
                      ifelse(length(x@print)>0,'printfile',';')),
              '[stock]',
              sprintf('stockfiles\t%s',
                      paste(sapply(x@stocks,function(x) x@stockname),
                            collapse='\t')),
              '[tagging]',
              ifelse(length(x@tags)>0,'tagfiles\ttags',';'),
              '[otherfood]',
              ifelse(length(x@otherfood)>0,'otherfoodfiles\totherfood',';'),
              '[fleet]',
              ifelse(length(x@fleets)>0,'fleetfiles\tfleets',';'),
              '[likelihood]',
              ifelse(length(x@likelihood)>0,
                     'likelihoodfiles\tlikelihood',';'),
              sep='\n'
              )
      write(main.text,file=sprintf('%s/main',loc))
      invisible(main.text)
            }
)

setMethod("write",
          signature(x = "gadget-spawning"),
          function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
                    append = FALSE, sep = " ") 
          {
            spawn.text <- 
              paste(sprintf('; spawning file created using Rgadget at %s',Sys.Date()),
                    sprintf('spawnsteps\t%s', x@spawnsteps),
                    sprintf('spawnareas\t%s', x@spawnareas),
                    sprintf('firstspawnyear\t%s', x@firstspawnyear),
                    sprintf('lastspawnyear\t%s',  x@lastspawnyear),
                    paste('spawnstocksandratios',x@spawnstocksandratio$stock,
                          x@spawnstocksandratio$ratio,sep='\t',collapse ='\n'),
                    sprintf('proportionfunction\t%s\t%s',
                          x@proportionfunction['func'],paste(x@proportionfunction[-1],collapse='\t')),
                    sprintf('mortalityfunction\t%s\t%s',
                            x@mortalityfunction['func'],paste(x@mortalityfunction[-1],collapse='\t')),
                    sprintf('weightlossfunction\t%s\t%s',
                            x@weightlossfunction['func'],paste(x@weightlossfunction[-1],collapse='\t')),
                    sprintf('recruitment\t%s\t%s',
                            x@recruitment['func'],paste(x@recruitment[-1],collapse='\t')),
                    
                    sprintf('stockparameters\t%s\t%s\t%s\t%s',
                            x@stockparameters$mean, 
                            x@stockparameters$std.dev,
                            x@stockparameters$alpha, 
                            x@stockparameters$beta),
                    sep = '\n')
              write(spawn.text,file = file)
          }
          )

if(FALSE){
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


}