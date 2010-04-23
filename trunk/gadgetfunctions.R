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
                       printfinal=NULL){
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
  run.string <- paste('gadget',switches)
  run.history <- try(system(run.string,intern=TRUE,ignore.stderr=TRUE))
#  if(!is.null(o))
#    o <- read.table(o,skip=7,header=TRUE)
#  invisible(list(run.history=run.history,output=o))
  invisible(run.history)
}
                       
##' Read gadget printfiles
##'
##' This function attempts to read in the gadget output files defined in
##' printfiles. This is a quick and dirty implementation that has been 
##' designed to read the defined examples, so it may work for some instances
##' and it may not. It assumes that the first line containing the fewest words 
##' is the line describing the column names and is the last comment line.
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

