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
      dir.create(sprintf('%s/AggFiles',file), showWarnings = FALSE)
      header <- paste(sprintf('; prey aggregation file for %s',x@name),
                      sprintf('; created using rgadget at %s', Sys.Date()),
                      sep = '\n')
      write(header,file = sprintf('%s/AggFiles/%s.prey.agg',file,x@name))
      write.table(x@preylengths,file = sprintf('%s/%s.prey.agg',file,x@name),
            col.names=FALSE,append=TRUE,
            quote=FALSE,sep='\t',row.names=FALSE)
      paste(sprintf('preylengths\t%s/Aggfiles/%s.prey.agg',file,x@name),
            sprintf('energycontent\t%s',x@energycontent),
            sep = '\n')    
    }
)

setMethod("write",
    signature(x = "gadget-stock"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        ref.head <- paste(sprintf('; refweight file for %s created using rgadget at %s',
                                  x@stockname,Sys.Date()),
                          paste(c('; ',names(x@refweight),collapse = '\t')),
                          sep = '\n')
        write(ref.head,file = sprintf('%s/Data/%s.refweigthfile',file,x@stockname))
        write.table(x@refweight,
                    file = sprintf('%s/Data/%s.refweigthfile',file,x@stockname),
                  col.names=FALSE,append=TRUE,
                  quote=FALSE,sep='\t',row.names=FALSE)
        lengths <- seq(300,900,by = 10)
        lenAgg <- data.frame(length = paste('len',tail(lengths,-1), sep = ''),
                             min = tail(lengths,-1),
                             max = head(lengths,-1)
                             )
        stock.text <- 
          paste(sprintf('; stock definition file for %s created using rgadget',x@stockname),
                sprintf('; at %s',Sys.Date()),
                ';',
                sprintf('stockname\t%s'x@stockname),
                sprintf('livesonareas\t%s',paste(x@livesonareas,collapse = '\n')),
                sprintf('minage\t%s',x@minage),
                sprintf('maxage\t%s',x@maxage),
                sprintf('minlength\t%s',x@minlength),
                sprintf('maxlength\t%s',x@maxlength),
                sprintf('dl\t%s',x@dl),
                sprintf('refweightfile\tData/%s.refweigthfile',x@stockname),
                sprintf('growthandeatlengths\tAggFiles/len.agg',)
    }
)

setMethod("write",
    signature(x = "gadget-predator"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        stop("Need a definition for the method here")
    }
)

setMethod("write",
    signature(x = "gadget-growth"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        stop("Need a definition for the method here")
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
        c(name = sprintf('%s\t%s',x@type,x@name),
          area = sprintf('livesonareas\t%s',x@livesonareas),
          mulit = sprintf('multiplicative\t%s',x@multiplicative),
          suit = sprintf('suitability\n%s',
                  paste(x@suitability$preyname,
                        x@suitability$func,
                        paste(x@suitability$parameters,collapse = '\t'))),
          empty = '; empty space -- move along nothing to see here',
          amount = sprintf('data/amount\t%s.amounts',x@name))

      if(x@type == 'quotafleet')
        fleet.text['empty'] <- 
          paste(sprintf('quotafunction\t%s',x@quotafunction),
                sprintf('biomasslevel\t%s',paste(x@biomasslevel,collapse = '\t')),
                sprintf('quotalevel\t%s',paste(x@quotalevel,collapse = '\t')),
                sep = '\n')
      else if(x@type == 'effortfleet')
        fleet.text['empty'] <- 
          sprintf('catchability\n%s',
                  paste(paste(x@catchability$stock,
                              x@catchability$catchabilty,sep='\t'),collapse = '\n'))
      
      write.table(x@amount,file=sprintf('%s/Data/%s.amount',file,x@name),
                  col.names=FALSE,
                  quote=FALSE,sep='\t',row.names=FALSE)
      if(file.exists(sprintf('%s/fleets',file))){
        write(paste(fleet.text,collapse=TRUE),file=sprintf('%s/fleets',file),
              append = TRUE)
      } else {
        write(paste(fleet.text,collapse=TRUE),file=sprintf('%s/fleets',file))
      }
      invisible(fleet.text)
    }
)


setMethod("write",
    signature(x = "gadget-main"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
      loc <- sprintf('%s/%s',file,x@model.name)
      dir.create(loc)
      ## writing ecosystem files
      write(x@area, file = sprintf('%s/area',loc))
      write(x@time, file = sprintf('%s/time',loc))
      if(length(x$print) > 0)
        write(x@print, file = sprintf('%s/printfile',loc))
      for(stock in x@stocks)
        write(stock,file = loc)
#      write(x@tags,file = sprintf('%s/tagfile',loc))
#      write(x@otherfood, file = sprintf('%s/otherfood',loc))
      for(fleet in x@fleets)
        write(fleet,file=loc)
      ## Likelihood files
      ##write(x@likelhood, file = sprintf('%s/likelihood'))
      
      main.text <-
        paste(sprintf('; main file for the %s model',x@model.name),
              sprintf('; created using rgadget at %s',Sys.Date()),
              'timefile\ttime',
              'areafile\ttime',
              sprintf('printfiles\t%s',
                      ifelse(length(x@print)>0,';','prinfile')),
              '[stock]',
              sprintf('stockfiles\t%s',
                      paste(sapply(x@stocks,function(x) x@stockname),
                            collapse='\t')),
              '[tagging]',
              ifelse(length(x@tags)>0,'tagfiles\ttags',''),
              '[otherfood]',
              ifelse(length(x@otherfood)>0,'otherfoodfiles\totherfood',''),
              '[fleet]',
              ifelse(length(x@fleets)>0,'fleetfiles\tfleets',''),
              '[likelihood]',
              ifelse(length(x@likelihood)>0,
                     'likelihoodfiles\tlikelihood',''),
              sep='\n'
              )
      write(main.text,file=sprintf('%s/main',loc))
      invisible(main.text)
            }
)

