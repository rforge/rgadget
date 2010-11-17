library(multicore)

##' Call GADGET
##'
##' This function sets up all necessary switches and call gadget from R
##' and attempts to read some of the output from gadget. This has currently
##' only been tested on linux and requires gadget to be in the users path.
##' The source code for gadget can be obtained from 
##' @param l perform a likelihood (optimising) model run
##' @param s perform a single (simulation) model run
##' @param n perform a network run (using paramin)
##' @param v display version information and exit
##' @param h display this help screen and exit
##' @param i name of the file containing the model parameters
##' @param opt name of the file containing the optimising parameters
##' @param main name of the main file (optional, as default it will
##' look for 'main'
##' @param m name of the file containing additional parameters
##' (optional)
##' @param p name of the file to which the parameter estimates should
##' be output to. Default is 'params.out'
##' @param o name of the file to which the likelihood-output should be
##' saved (optional).
##' @param print Number. print -o output every <number> iterations.
##' @param precision set the precision to <number> in output files
##' @param log Name of the file to which the logging information
##' should be saved.
##' @param printinitial Name of the file to which the initial model
##' information sould be saved.
##' @param printfinal Name of the file to which the final model
##' information sould be saved.
##' @param gadget.exe path to the gadget executable, if it is not in the path
##' @return the run history
callGadget <- function(l=NULL,
                       s=NULL,
                       n=NULL,
                       v=NULL,
                       h=NULL,
                       i=NULL,
                       opt=NULL,
                       main=NULL,
                       m=NULL,
                       p=NULL,
                       o=NULL,
                       print=NULL,
                       precision=NULL,
                       log=NULL,
                       printinitial=NULL,
                       printfinal=NULL,
                       gadget.exe='gadget'){
  switches <- paste(ifelse(is.null(l),'','-l'),
                    ifelse(is.null(s),'','-s'),
                    ifelse(is.null(n),'','-n'),
                    ifelse(is.null(v),'','-v'),
                    ifelse(is.null(h),'','-h'),
                    ifelse(is.null(i),'',paste('-i',i)),
                    ifelse(is.null(opt),'',paste('-opt',opt)),
                    ifelse(is.null(main),'',paste('-main',main)),
                    ifelse(is.null(m),'',paste('-m',m)),
                    ifelse(is.null(p),'',paste('-p',p)),
                    ifelse(is.null(o),'',paste('-o',o)),
                    ifelse(is.null(print),'',paste('-print',print)),
                    ifelse(is.null(precision),'',paste('-precision',precision)),
                    ifelse(is.null(log),'',paste('-log',log)),
                    ifelse(is.null(printinitial),'',
                           paste('-printinitial',printinitial)),
                    ifelse(is.null(printfinal),'',
                           paste('-printfinal',printfinal)))
  run.string <- paste(gadget.exe,switches)
  run.history <- try(system(run.string,intern=TRUE,ignore.stderr=TRUE))
#  if(!is.null(o))
#    o <- read.table(o,skip=7,header=TRUE)
#  invisible(list(run.history=run.history,output=o))
  invisible(run.history)
}
                       

##' This function attempts to read in the gadget output files defined in
##' printfiles. This is a quick and dirty implementation that has been 
##' designed to read the defined examples, so it may work for some instances
##' and it may not. It assumes that the first line containing the fewest words 
##' is the line describing the column names and is the last comment line.
##' @title Read gadget printfiles
##' @param location the folder containing the printfiles
##' @return a list containing the data that has been read in.
read.printfiles <- function(location='.'){
  read.printfile <- function(file){
     tmp <- readLines(file)
     lengths <- sapply(strsplit(tmp,' '),length)
     skip <- min((1:length(tmp))[lengths==min(lengths)])
     header <-
       strsplit(paste(strsplit(tmp[skip],' ')[[1]][2:min(lengths)],
                      collapse='.'),'-')[[1]]
     data <- read.table(file,skip=skip,header=FALSE)
     names(data) <- header
     return(data)
  }
  out.files <- try(system(paste('ls',location),
                          intern=TRUE,ignore.stderr=TRUE))
  printfiles <- within(list(),
                       for(printfile in out.files){
                         assign(printfile,
                                read.printfile(paste(location,
                                                     printfile,
                                                     sep='/')))
                       })
  printfiles$printfile <- NULL
  return(printfiles)
}
##' This functions reads the likelihood (input) file for gadget
##' @title Read likelihood
##' @param file likelihood file
##' @return object of class gadget.likelihood, i.e. a list containing the various likelihood components
##' @author Bjarki Þór Elvarsson
read.gadget.likelihood <- function(file='likelihood'){
  lik <- readLines(file)
  lik <- lik[!grepl(';',substring(lik,1,1))]
  lik <- sapply(strsplit(lik,';'),function(x) x[1])
  comp.loc <- grep('component',lik)
  name.loc <- comp.loc+3
  weights <- NULL
  common <- c('name','weight','type','datafile')
  tmp.func <- function(comp){
    loc <- grep(comp,lik[name.loc])  
    dat <- NULL
    for(dd in loc){
      if(dd < length(comp.loc)) {
        restr <- (comp.loc[dd] + 1):(comp.loc[dd+1]-1)
      } else {
        restr <- 1:length(lik) > comp.loc[dd]
      }
      tmp <- sapply(strsplit(sapply(strsplit(lik[restr],' '),
                                    function(x) {
                                      paste(x[!(x==''|x=='\t')],
                                            collapse=' ')
                                  }),' '),
                    function(x) as.character(x))
      dat <- rbind(dat, tmp)
    }
    names.dat <- head(dat,1)
    dat <- as.data.frame(dat,stringsAsFactors=FALSE)[2*(1:length(loc)),]
    names(dat) <- names.dat
    row.names(dat) <- dat$name
    if(comp=='understocking'){
      dat$datafile <- ''
    }
    weights <<-  rbind(weights,
                       dat[common])
    if(comp=='understocking'){
      dat$datafile <- NULL
    }
    dat$weight <- NULL
    return(dat)
  } 
  ## understocking
  penalty <- tmp.func('penalty')
  understocking <- tmp.func('understocking')
  surveyindices <- tmp.func('surveyindices')
  catchdistribution <- tmp.func('catchdistribution')
  catchstatistics <- tmp.func('catchstatistics')
  weights$weight <- as.numeric(weights$weight)
  likelihood <- list(weights=weights,penalty=penalty,understocking=understocking,
                      surveyindices=surveyindices,
                      catchdistribution=catchdistribution,catchstatistics=catchstatistics)
  class(likelihood) <- c('gadget.likelihood',class(likelihood))
  return(likelihood)
}
##' Write a likelihood object to file
##' @title Write likelihood
##' @param lik object of class gadget.likelihood
##' @param file name of the likelihood file
##' @param location folder
##' @return character string corresponding to the likelihood file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.likelihood <- function(lik,file='likelihood',location='.'){
  lik.text <- '; Likelihood file - created in Rgadget'
#  comp <- '[component]'
  weights <- lik$weights
  lik$weights <- NULL
  weights$type <- NULL
  weights$datafile <- NULL
  for(comp in lik){
    comp <- merge(weights,comp,by='name',sort=FALSE)
    
    comp.text <- paste(names(comp),t(comp))
    dim(comp.text) <- dim(t(comp))
    comp.text <- rbind('[component]',comp.text,';')
    lik.text <- paste(lik.text,
                      paste(comp.text,
                            collapse='\n'),
                      sep='\n')                   
  }
  write(lik.text,file=paste(location,file,sep='/'))
  invisible(lik.text)
}
##' Read gadget's main file
##' @title Read main
##' @param file main file location
##' @return object of class gadget.main
##' @author Bjarki Þór Elvarsson
read.gadget.main <- function(file){
  main <- readLines(file)
  main <- main[!grepl(';',substring(main,1,1))]
  main <- sapply(strsplit(main,';'),function(x) x[1])
  main <- clear.spaces(main)
  main <- as.data.frame(sapply(main[sapply(main,length)!=1],function(x) x),
                        stringsAsFactors=FALSE)
  names(main) <- main[1,]
  main <- main[2,]
  row.names(main) <- 1
  class(main) <- c('gadget.main',class(main))
  return(main)
}
##' Write gadget.main object to file
##' @title Write main
##' @param main gadget.main object
##' @param file name of main file 
##' @param location folder
##' @return text of the main file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.main <- function(main,file='main',location='.'){
  main.text <- '; main file for gadget - created in Rgadget'
  if(is.null(main$printfiles)){
    main$printfiles <- '; no printfile supplied'  
  }
  main.text <-
    paste(main.text,
          paste('timefile',main$timefile),
          paste('areafile',main$areafile),
          paste('printfiles',main$printfiles),
          '[stock]',
          paste('stockfiles',main$stockfiles),
          ifelse(is.null(main$tagfiles),
                 '[tagging]',
                 paste('[tagging]\ntagfiles',main$tagfiles)),
          ifelse(is.null(main$otherfoodfiles),
                 '[otherfood]',
                 paste('[otherfood]\notherfoodfiles',main$otherfoodfiles)),
          ifelse(is.null(main$likelihoodfiles),
                 '[fleet]',
                 paste('[fleet]\nfleetfiles',main$fleetfiles)),
          '[likelihood]',
          paste('likelihoodfiles',main$likelihoodfiles),
          sep='\n')
  write(main.text,paste(location,file,sep='/'))
  invisible(main.text)
}
  
##' Clear tab and spaces from a string and return a list or a matrix of values 
##' @title Clear spaces
##' @param text string 
##' @return list or matrix containing the (non-empty) values from the string
##' @author Bjarki Þór Elvarsson
clear.spaces <- function(text){
  sapply(strsplit(sapply(strsplit(text,'[ \t]'),
                         function(x) {
                           paste(x[!(x==''|x=='\t')],
                                 collapse=' ')
                         }),' '),
         function(x) x)
}
##' Read gadget parameter file
##' @title Read param
##' @param file parameter file
##' @param location folder
##' @return dataframe
##' @author Bjarki Þór Elvarsson
read.gadget.parameters <- function(file='params.in',location='.'){
  params <- read.table(paste(location,file,sep='/'),header=TRUE,
                       comment.char=';')
  class(params) <- c('gadget.parameters',class(params))
  return(params)
}
##' Write gadget input parameters
##' @title Write params
##' @param params params dataframe
##' @param file a string naming the file to write to
##' @param location a string naming the folder where the file is to be written
##' @return a string containing the text of the params file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.parameters <- function(params,file='params.out',location='.'){
  input.text <-
    paste("; input file for the gadget model",
          "; created automatically from Rgadget",
          paste(names(params),collapse='\t'),
          sep='\n')
  write(input.text,paste('.',location,file,sep='/'))
  write.table(params,file=paste('.',location,file,sep='/'),
              quote=FALSE, row.names=FALSE, col.names=FALSE,
              append=TRUE, sep="\t")
}
##' An implementation of the iterative reweigthing of likelihood components
##' in gadget.  If one (or more) components, other than understocking and
##' penalty, is 0 then the gadget optimisation with the final weights will
##' not be completed.
##' @title Iterative reweighting
##' @param main.file a string containing the location of the main file
##' @param gadget.exe a string containing the location of the gadget
##' executable
##' @param params.file a string containing the location of the input
##' parameters
##' @param rew.sI logical, should survey indices be iteratively reweighted (TRUE) or estimated using a linear model.
##' @return a matrix containing the weights of the likelihood components at each iteration.
##' @author Bjarki Þór Elvarsson
gadget.iterative <- function(main.file='main',gadget.exe='gadget',
                             params.file='params.in',rew.sI=FALSE) {
  main <- read.gadget.main(main.file)
  likelihood <- read.gadget.likelihood(main$likelihoodfiles)
  params.in <- read.gadget.parameters(params.file,'.')
  main.init <- main
  main.init$printfile <- NULL
  main.init$likelihoodfiles <- 'likelihood.init'
  write.gadget.likelihood(likelihood,file='likelihood.init','.')
  write.gadget.main(main.init,file='main.init')
  callGadget(s=1,main='main.init',o='lik.init',i=params.file,gadget.exe=gadget.exe)
  SS <- read.gadget.SS('lik.init')
  ## degrees of freedom approximated by the number of datapoints
  lik.dat <- read.gadget.data(likelihood)
  
  restr <- !(likelihood$weights$type %in% c('penalty','understocking'))
  if(!rew.sI){
    restr <- restr&(likelihood$weights$type != 'surveyindices')
    sIw <- sI.weights(lik.dat)
  }
  num.comp <- sum(restr)
  ## Base run (with the inverse SS as weights)
  main.base <- main.init
  main.base$likelihoodfiles <- 'likelihood.base'
  write.gadget.main(main.base,file='main.base')
  likelihood.base <- likelihood
  likelihood.base$weights$weight[restr] <- 1/SS[restr]
  if(!rew.sI)
    likelihood.base$weights$weight[likelihood.base$weights$type=='surveyindices'] <- sIw

  ## Gadget set up stuff, needed for each component
  run.iterative <- function(comp){
    likelihood <- likelihood.base
    which.comp <- likelihood$weights$name %in% comp
    likelihood$weights$weight[which.comp] <-
      10000*likelihood$weights$weight[which.comp]
    comp <- paste(comp,collapse='.')
    write.gadget.likelihood(likelihood,file=paste('likelihood',comp,sep='.'))
    main <- main.base
    main$likelihoodfiles <- paste('likelihood',comp,sep='.')
    write.gadget.main(main,file=paste('main',comp,sep='.'))
    callGadget(l=1,
               main=paste('main',comp,sep='.'),
               i=params.file,
               p=paste('params',comp,sep='.'),
               opt='optinfofile',
               gadget.exe=gadget.exe)
    Callgadget(s=1,
               main=paste('main',comp,sep='.'),
               i=paste('params',comp,sep='.'),
               o=paste('lik',comp,sep='.'),
               gadget.exe=gadget.exe)
    SS.comp <- read.gadget.SS(paste('lik',comp,sep='.'))
    return(SS.comp)
  }
  
  run.string <- c('base',likelihood.base$weights$name[restr])
  if(!rew.sI){
    run.string <- as.list(run.string)
    run.string$SI <-
      likelihood.base$weights$name[likelihood.base$weights$type=='surveyindices']
  }
  Res <- mclapply(run.string,run.iterative)
  SS.table <- as.data.frame(t(sapply(res,function(x) x)))
  names(SS.table) <- likelihood.base$weights$name
  if(FALSE){
    not.rew <- sum(!restr)
    final.SS <- diag(SS.table[-1,restr])
    df <- rep(0,num.comp)
    for(i in 1:num.comp)
      df[i] <- lik.dat$df[[likelihood$weights$type[not.rew+i]]][[likelihood$weights$name[not.rew+i]]]
    final.weights <- df/final.SS
    main.final <- main.base
    main.final$likelihoodfiles <- 'likelihood.final'
    write.gadget.main(main.final,'main.final')
    likelihood.final <- likelihood.base
    likelihood.final$weights$weight[restr] <- final.weights
    write.gadget.likelihood(likelihood.final,file='likelihood.final')
    comp <- 'final'
    callGadget(l=1,
               main=paste('main',comp,sep='.'),
               i=params.file,
               p=paste('params',comp,sep='.'),
               opt='optinfofile',
               gadget.exe=gadget.exe)
  }
  return(list(res=res,num.comp=num.comp,SS=SS.table,lik.dat=lik.dat))
}

sI.weights <- function(lik.dat){
  dat <- NULL
  for(comp in lik.dat$dat$surveyindices){
    dat <- rbind(dat,comp)
  }
  dat$comp <- rep(names(lik.dat$dat$surveyindices),lik.dat$df$surveyindices)
  dat$y <- log(dat$number)
  dat$year <- as.factor(dat$year)
  fit <- lm(y~year+length,dat)
  weights <- (lik.dat$df$surveyindices -
              tapply(dat$length,dat$comp,function(x) length(unique(x))))/
    tapply(resid(fit),dat$comp,function(x) sum(x^2))
  return(weights)
}

##' Read the values of likelihood components from the likelihood output
##' @title Read SS
##' @param file a string containing location the likelihood output
##' @param location folder
##' @return vector of likelihood values
##' @author Bjarki Þór Elvarsson
read.gadget.SS <- function(file='lik.out',location='.'){
  lik.out <- readLines(paste(location,file,sep='/'))
  SS <- as.numeric(clear.spaces(strsplit(lik.out[length(lik.out)],'\t\t')[[1]][2]))
  return(SS)
}

##' Read data used by the various components
##' @title Read likelihood data
##' @param likelihood 
##' @return list of dataframes and degress of freedom
##' @author Bjarki Þór Elvarsson
read.gadget.data <- function(likelihood){
  read.agg <- function(x){
    if(!is.null(x))
      return(sapply(strsplit(readLines(x),'[\t ]'),function(x) x[1]))
    else
      return(NULL)
  }
  read.func <- function(x){
    x <- as.data.frame(t(x),stringsAsFactors=FALSE)
    dat <- read.table(x$datafile,comment.char=';')
    area.agg <- read.agg(x$areaaggfile)
    age.agg <- read.agg(x$ageaggfile)
    len.agg <- read.agg(x$lenaggfile)
      
    if(x$type=='catchdistribution'){
      names(dat) <- c('year','step','area','age','length','number')
    }
    if(x$type=='catchstatistics'){
      if(x[['function']] %in%
         c('lengthcalcstddev','weightnostddev','lengthnostddev'))
        names(dat) <- c('year','step','area','age','number','mean')
      if(x[['function']] %in% c('lengthgivenstddev','weightgivenstddev','lengthgivenvar'))
        names(dat) <- c('year','step','area','age','number','mean','stddev') 
    }
    if(x$type=='stockdistribution'){
      names(dat) <- c('year','step','area','stock','age','length','number')
    }
    if(x$type=='surveyindices'){
      if(x$sitype %in% c('lengths','fleets') )
        names(dat) <- c('year','step','area','length','number')
      if(x$sitype=='ages')
        names(dat) <- c('year','step','area','age','number')
      if(x$sitype=='acoustic')
        names(dat) <- c('year','step','area','survey','acoustic')
      if(x$sitype=='effort')
        names(dat) <- c('year','step','area','fleet','effort')
    }
    if(x$type == 'surveydistribution'){
      names(dat) <- c('year','step','area','age','length','number')
    }
    if(x$type=='stomachcontent'){
      names(dat) <- c('year','step','area','predator','prey','ratio')
    }
    if(x$type=='recaptures'){
      names(dat) <- c('tagid','year','step','area','length','number')
    }
    if(x$type=='recstatistics'){
      if(x[['function']]=='lengthgivenstddev')
        names(dat) <- c('tagid','year','step','area','number','mean','stddev')
      else
        names(dat) <- c('tagid','year','step','area','number','mean')
    }
    if(x$type=='catchinkilos'){
      if(x$aggregationlevel==1)
        names(dat) <- c('year','area','fleet','biomass')
      else
        names(dat) <- c('year','step','area','fleet','biomass')
    }
    
    restr.area <- (dat$area %in% area.agg)
    if(length(restr.area)==0)
      restr.area <- TRUE
    restr.age <- (dat$age %in% age.agg)
    if(length(restr.age)==0)
      restr.age <- TRUE
    restr.len <- (dat$length %in% len.agg)
    if(length(restr.len)==0)
      restr.len <- TRUE
    dat <- dat[restr.area&restr.age&restr.len,]
    return(dat)
  }
  lik.dat <- within(list(),
                    for(comp.type in
                        names(likelihood[!(names(likelihood) %in%
                                           c('weights','penalty','understocking'))])) {
                      assign(comp.type,
                             apply(likelihood[[comp.type]],1,read.func))
                    }
                    
                    )
  lik.dat$comp.type <- NULL
  df <- sapply(lik.dat,function(x) sapply(x,function(x) dim(x[x[,dim(x)[2]]>0,])[1]))
  return(list(dat=lik.dat,df=df))
}

##' Read optinfo parameters from file
##' @title Read gadget  
##' @param file location of the optinfofile
##' @return optinfo object
##' @author Bjarki Þór Elvarsson
read.gadget.optinfo <- function(file='optinfofile'){
  optinfo <- readLines(file)
  optinfo <- na.omit(sapply(strsplit(optinfo,';'),function(x) x[1]))
  simann <- (1:length(optinfo))[(optinfo == '[simann]')]
  hooke <- (1:length(optinfo))[(optinfo == '[hooke]')]
  bfgs <- (1:length(optinfo))[(optinfo == '[bfgs]')]

  vars <- c(simann-1,hooke-1,bfgs-1,length(optinfo))
  simann.end <- min(vars[vars>simann])
  hooke.end <-  min(vars[vars>hooke])
  bfgs.end <- min(vars[vars>bfgs])
  tmp.func <- function(start,end){
    x <-  as.data.frame(clear.spaces(optinfo[start:end]),
                        stringsAsFactors=FALSE)
    names(x) <- x[1,]
    x <- x[2,]
    return(x)
  }
  optinfo <- list(simann = tmp.func(simann+1,simann.end),
                  hooke = tmp.func(hooke+1,hooke.end),
                  bfgs = tmp.func(bfgs+1,bfgs.end))
  class(optinfo) <- c('gadget.optinfo',class(optinfo))
  return(optinfo)
}
##' Write optinfo to file
##' @title Write gadget optinfo
##' @param optinfo optinfo object
##' @param file file
##' @param location location
##' @return text of the optinfofile (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.optinfo<-function(optinfo,file='optinfofile',location=''){
  opt.text <- 
    paste("; optimisation file for the gadget example",
          "; created automatically from R-gadget",
          sep='\n')
  for(comp in names(optinfo)){
    opt.text <-
      paste(opt.text,
            sprintf('[%s]',comp),
            paste(names(optinfo[[comp]]),
                  optinfo[[comp]],
                  sep='\t\t',collapse='\n'),
            sep='\n')
  }
  write(opt.text,paste(location,file,sep='/'))
  invisible(opt.text)
}


