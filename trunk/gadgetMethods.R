setMethod("write",
    signature(x = "gadget-time"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        stop("Need a definition for the method here")
    }
)

setMethod("write",
    signature(x = "gadget-area"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        stop("Need a definition for the method here")
    }
)

setMethod("write",
    signature(x = "gadget-stock"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        stop("Need a definition for the method here")
    }
)

setMethod("write",
    signature(x = "gadget-prey"),
    function (x, file = "data", ncolumns = if (is.character(x)) 1 else 5, 
        append = FALSE, sep = " ") 
    {
        stop("Need a definition for the method here")
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
        stop("Need a definition for the method here")
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
      write(x@tags,file = sprintf('%s/tagfile',loc))
      write(x@otherfood, file = sprintf('%s/otherfood',loc))
      for(fleet in x@fleets)
        write(fleet,file=loc)
      write(x@likelhood, file = sprintf('%s/likelihood'))
      
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

