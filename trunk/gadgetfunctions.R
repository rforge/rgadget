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
##' @param main name of the main file (optional, as default it will look for 'main'
##' @param m name of the file containing additional parameters (optional)
##' @param p name of the file to which the parameter estimates should be output to. Default is 'params.out'
##' @param o name of the file to which the likelihood-output should be saved (optional).
##' @param print Number. print -o output every <number> iterations.
##' @param precision set the precision to <number> in output files
##' @param log Name of the file to which the logging information should be saved.
##' @param printinitial Name of the file to which the initial model information sould be saved.
##' @param printfinal Name of the file to which the final model information sould be saved.
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
read.printfiles <- function(location){
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

read.gadget.likelihood <- function(file){
  lik <- readLines(file)
  lik <- lik[!grepl(';',substring(lik,1,1))]
  lik <- sapply(strsplit(lik,';'),function(x) x[1])
  comp.loc <- grep('component',lik)
  name.loc <- comp.loc+3
  weights <- NULL
  common <- c('name','weight','type')
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
    weights <<-  rbind(weights,
                   dat[common])
    dat$weight <- NULL
    return(dat)
  } 
  ## understocking
  penalty <- tmp.func('penalty')
  understocking <- tmp.func('understocking')
  surveyindices <- tmp.func('surveyindices')
  catchdistribution <- tmp.func('catchdistribution')
  catchstatistics <- tmp.func('catchstatistics')
  
  likelihood <- list(weights=weights,penalty=penalty,understocking=understocking,
                      surveyindices=surveyindices,
                      catchdistribution=catchdistribution,catchstatistics=catchstatistics)
  class(likelihood) <- c('gadget.likelihood',class(likelihood))
  return(likelihood)
}

write.gadget.likelihood <- function(lik,file,location){
  lik.text <- '; Likelihood file - created in Rgadget'
#  comp <- '[component]'
  weights <- lik$weights
  lik$weights <- NULL
  weights$type <- NULL
  for(comp in lik){
    comp <- merge(weights,comp,by='name')
    
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
  

clear.spaces <- function(text){
  sapply(strsplit(sapply(strsplit(text,' '),
                         function(x) {
                           paste(x[!(x==''|x=='\t')],
                                 collapse=' ')
                         }),' '),
         function(x) x)
}

read.gadget.parameters <- function(file='params.in',location='.'){
  params <- read.table(paste(location,file,sep='/'),header=TRUE,
                       comment.char=';')
  class(params) <- c('gadget.parameters',class(params))
  return(params)
}

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

gadget.iter <- function(main.file='main',gadget.exe='gadget',params.file='params.in') {
  main <- read.gadget.main(main.file)
  likelihood <- read.gadget.likelihood(main$likelihoodfiles)
  params.in <- read.gadget.parameters(params.file,'.')
  main.init <- main
  main.init$printfile <- NULL
  write.gadget.main(main.init,file='main.init')
  callGadget(s=1,main='main.init',o='lik.init',i=params.file,gadget.exe=gadget.exe)
  SS <- read.gadget.SS('lik.init')
  restr <- !(likelihood$weights$type %in% c('penalty','understocking'))
  num.comp <- sum(restr)
  ## base run (with the inverse SS as weights)
  main.base <- main.init
  main.base$likelihoodfiles <- 'likelihood.base'
  write.gadget.main(main.base,file='main.base')
  likelihood.base <- likelihood
  likelihood.base$weights[restr] <- 1/SS[restr]
  write.gadget.likelihood(likelihood.base,file='likelihood.base')
  callGadget(l=1,main='main.base',i=params.file,p='params.base',gadget.exe=gadget.exe)
  callGadget(s=1,main='main.base',i='params.base',o='lik.base',gadget.exe=gadget.exe)
  SS.base <- read.gadget.SS('lik.base')
  run.iterative <- function(comp){
    likelihood <- likelihood.base
    which.comp <- likelihood$weights$name==comp
    likelihood$weights$weight[wich.comp] <- 10000*likelihood$weights$weight[wich.comp]
    write.gadget.likelihood(likelihood,file=paste('likelihood',comp,sep='.'))
    main <- main.base
    main$likelihoodfiles <- paste('likelihood',comp,sep='.')
    write.gadget.main(main,file=paste('main',comp,sep='.'))
    callGadget(l=1,
               main=paste('main',comp,sep='.'),
               i=params.file,
               p=paste('params',comp,sep='.'),
               gadget.exe=gadget.exe)
    callGadget(s=1,
               main=paste('main',comp,sep='.'),
               i=paste('params',comp,sep='.'),
               o=paste('lik',comp,sep='.'),
               gadget.exe=gadget.exe)
    SS.comp <- read.gadget.SS(paste('lik',comp,sep='.'))
    return(SS.comp)
  }
  
  res <- mclapply(likelihood.base$weights$name[restr],run.iterative)
  return(res)
}

read.gadget.SS <- function(file='lik.out',location='.'){
  lik.init <- readLines(paste(location,file,sep='/')
  SS <- as.numeric(clear.spaces(strsplit(lik.out[length(lik.out)],'\t\t')[[1]][2]))
  return(SS)
}
