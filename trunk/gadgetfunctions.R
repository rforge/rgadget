library(reshape2)
library(plyr)
library(multicore)
library(doMC)
library(foreach)
#registerDoMC()
#source('gadgetFileIO.R')

##' This function sets up all necessary switches and calls gadget from R
##' and attempts to read the runtime output from gadget. This has currently
##' only been tested on unix based platforms (but should in principle work on
##' windows).
##' The source code for gadget can be obtained from http://www.hafro.is/gadget
##'
##' Gadget is a flexible and powerful tool for creating ecosystem models.
##' The program was developed for modelling marine ecosystems in a fisheries
##' management and biology context, however there is nothing in the program
##' that restricts it to fish , and models have been developed to examine
##' marine mammal populations. Indeed there is nothing to demand that the
##' populations being considered are marine, or even aquatic, in nature.
##' Gadget allows you to include a number of features into your model:
##' One or more species, each of which may be split into multiple stocks;
##' multiple areas with migration between areas; predation between and within
##' species; maturation; reproduction and recruitment; multiple commercial and
##' survey fleets taking catches from the populations.
##' Gadget does two separate, but related things. Firstly it takes a model
##' specification and performs a simulation using that set up. The model
##' specification dictates the form of the equations to be used to describe
##' growth, recruitment, fleet selectivity and so on, and the exact parameters
##' to be used in these equations. Gadget will calculate model population and
##' catches through time for your given set up. Note that to do this it does
##' not use real-world data (except possibly overall catch tonnage). The
##' program then compares various aspects of the modelled catches with
##' real-world data from actual catches, and produces numeric likelihood
##' scores measuring how well the model matched each data set. The program
##' also computes a single overall likelihood score. This is a single number
##' representing the 'goodness of fit' between the simulation and the data.
##' It is worth repeating this point. Gadget runs a complete simulation without
##' reference to any data. It then compares the modelled and real catches, and
##' produces a score evaluating the fit between the two.
##' If Gadget is called upon to optimise a model solution it simply iterates
##' this process, trying different parameter values for each iteration. The
##' 'best fit' will be produced by the parameter set which produces a model
##' with the lowest overall likelihood score. There are several different
##' optimisation methods utilised.
##' @title Call GADGET
##' @param l performs a likelihood (optimising) model run
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
##' information should be saved.
##' @param printfinal Name of the file to which the final model
##' information should be saved.
##' @param gadget.exe path to the gadget executable, if it is not in
##' the path
##' @param PBS Logical, should, instead of running gadget directly,
##' a pbs script be
##' generated that can be submitted to a cluster queue (defaults to FALSE).
##' @param qsub.script The name of the qsub script that can be generated, if
##' desired. As with the PBS script R tries to set the permission to 777
##' (not wether or not this works on windows).
##' @param PBS.name Name of the pbs script (.sh will be appended).
##' @param qsub.output The directory where the output from the script is stored
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
                       gadget.exe='gadget',
                       PBS=FALSE,
                       qsub.script=NULL,
                       PBS.name='run',
                       qsub.output='output'
                       ){
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
  if(!PBS){
    run.history <- try(system(run.string,intern=TRUE,ignore.stderr=TRUE))
  } else {
    if(file.exists(sprintf('%s.sh',PBS.name))){
      write(run.string, file=sprintf('%s.sh',PBS.name),append=TRUE)
      qsub.script <- NULL
    } else {
      PBS.header <-
        paste('#!/bin/bash',
              sprintf('# job file for pbs queue created by Rgadget at %s',
                     date()),
              '# Copy evironment, join output and error, medium queue:',
              '#PBS -V',
              '#PBS -j oe',
              '#PBS -q medium',
              '#PBS -l cput=60:00:00',
              '#PBS -m n',
              '',
              '# Go to the directory where the job was submitted from',
              'cd $PBS_O_WORKDIR',
              '',
              '# run gadget',
              sep='\n')

      PBS.script <- paste(PBS.header,
                          run.string,
                          sep='\n')
      write(PBS.script, file=sprintf('%s.sh',PBS.name))
      Sys.chmod(sprintf('%s.sh',PBS.name),mode = '0777')
      if(!is.null(qsub.script)){
        dir.create(qsub.output)
        qsub.string <-
          sprintf('# %1$s\nqsub -N gadget-%2$s -o %3$s/%4$s.txt %2$s.sh \n',
                  date(),PBS.name,qsub.output,gsub('/','.',PBS.name))
        if(file.exists(qsub.script)){
          write(qsub.string,file=qsub.script,append=TRUE)
        } else {
          header <-
            paste('#!/bin/bash',
                  sprintf('# created by Rgadget at %s',date()),
                  sep='\n')
          write(paste(header,qsub.string,sep='\n'),file=qsub.script)
          Sys.chmod(qsub.script,mode = '0777')
        }
      }
    }
    
    run.history <- NULL
  }
  
  invisible(run.history)
}

##' <description>
##'
##' <details>
##' @title 
##' @param i 
##' @param func 
##' @param opt 
##' @param network 
##' @param o 
##' @param scale 
##' @param condor 
##' @param paramin.exe 
##' @return 
##' @author Bjarki Thor Elvarsson
callParamin <- function(i='params.in',
                        func='gadget -s -n',
                        opt='optinfo',
                        network='network',
                        o='params.out',
                        scale=NULL,
                        condor=NULL,
                        paramin.exe='paramin'){
  switches <- paste(ifelse(is.null(i),'',paste('-i',i)),
                    ifelse(is.null(opt),'',paste('-opt',opt)),
                    ifelse(is.null(o),'',paste('-o',o)),
                    ifelse(is.null(scale),'',paste('-scale',scale)),
                    ifelse(is.null(condor),'','-condor'),
                    ifelse(is.null(network),'',paste('-network',network)),
                    ifelse(is.null(func),'',paste('-function',func)))
  run.string <- paste(paramin.exe,switches)
  run.history <- try(system(run.string,intern=TRUE,ignore.stderr=TRUE))
  invisible(run.history)
}


##' An implementation of the iterative reweigthing of likelihood components
##' in gadget. It analyzes a given gadget model and, after a series of
##' optimisations where each likelihood component is heavily weigthed,
##' suggests a weigthing for the components based on the respective variance.
##' If one (or more) components, other than understocking and
##' penalty, are 0 then the gadget optimisation with the final weights will
##' not be completed.
##' 
##' In Taylor et. al an objective reweighting scheme for likelihood components 
##' is described for cod in Icelandic waters. The authors nota that the issue
##' of component weighting has been discussed for some time, as the data
##' sources have different natural scales (e.g g vs. kg) that should not
##' affect the outcome. A simple heuristic, where the weights are 
##' the inverse of the initial sums of squares for the respective component
##' resulting in an initials score equal to the number of components, is
##' therfor often used. This has
##' the intutitive advantage of all components being normalised. There is
##' however a drawback to this since the component scores, given the initial
##' parametrisation, are most likely not equally far from their respective
##' optima resulting in sub-optimal weighting.
##' The iterative reweighting heuristic tackles this problem by optimising
##' each component separately in order to determine the lowest possible value
##' for each component. This is then used to determine the final weights.
##' The resoning for this approach is as follows:
##' Conceptually the likelihood components can be thought of as residual sums
##' of squares, and as such their variance can be esimated by dividing the
##' SS by the degrees of freedom. The optimal weighting strategy is the inverse
##' of the variance.
##' Here the iteration starts with assigning the inverse SS as the initial
##' weight, that is the initial score of each component when multiplied with
##' the weight is 1. Then an optimisation run for each component with the intial
##' score for that component set to 10000. After the optimisation run
##' the inverse of the resulting SS is multiplied by the effective number of
##' datapoints and used as the final weight for that particular component.
##' The effective number of datapoints is used as a proxy for the degrees of
##' freedom is determined from the number of non-zero datapoints. This is viewed
##' as satisfactory proxy when the dataset is large, but for smaller datasets
##' this could be a gross overestimate. In particular, if the surveyindices
##' are weigthed on their own while the yearly recruitment is esimated they
##' could be overfitted. If there are two surveys within the year Taylor et. al
##' suggest that the corresponding indices from each survey are weigthed
##' simultaneously in order to make sure that there are at least two measurement for each
##' yearly recruit, this is done through component grouping which is implemented. Another approach, which is also implemented,
##' for say a single survey fleet the weight for each index component is
##' estimated from a model of the form
##' \deqn{\log(I_{lts}) = \mu + Y_t + \lambda_l + \Sigma_s + \epsilon_{lts}}{%
##' log(I_lts) = mu + Y_t + lambda_l + Sigma_s + e_lts}
##' where the residual term, \eqn{\epsilon_{lts}}{e_lts}, is independent normal
##' with variance \eqn{\sigma_{ls}^2}{sigma_ls^2}. The inverse of the estimated
##' variance from the above model as the weights between the surveyindices.
##' After these weights have been determined all surveyindices are weighted
##' simultaneously. 
##' @title Iterative reweighting
##' @param main.file a string containing the location of the main file
##' @param gadget.exe a string containing the location of the gadget
##' executable
##' @param params.file a string containing the location of the input
##' parameters
##' @param rew.sI logical, should survey indices be iteratively
##' reweighted (TRUE) or estimated using a linear model.
##' @param run.final logical should the final optimisation be run
##' (DEBUG)
##' @param resume.final logical should the final optimisation be
##' resumed (DEBUG)
##' @param wgts a string containing the path the folder where the
##' interim weighting results should be stored. 
##' @param grouping a list naming the groups of components that should be reweighted together. 
##' @param optinfofile optinfofile used in the reweighting
##' @param PBS Logical, should the gadget runs be defined to be run in pbs
##' scripts (defaults to FALSE).
##' @param qsub.script Name of cluster submission script.
##' @param run.base should the base (inverse initial SS) parameters be estimated
##' @param run.serial should the weighting run be run in parallel (used in
##' bootstrap). 
##' @return a matrix containing the weights of the likelihood components at each iteration (defaults to FALSE).
##' @author Bjarki Þór Elvarsson
gadget.iterative <- function(main.file='main',gadget.exe='gadget',
                             params.file='params.in',
                             rew.sI=FALSE,
                             run.final=TRUE,
                             resume.final=FALSE,
                             wgts = 'WGTS',
                             grouping = NULL,
                             optinfofile='optinfofile',
                             PBS = FALSE,
                             qsub.script = NULL,
                             run.base=FALSE,
                             run.serial = FALSE 
                             ) {
  ## store the results in a special folder to prevent clutter
  dir.create(wgts,showWarnings=FALSE)
  
  ##' Read the values of likelihood components from the likelihood output
  ##' @title Read SS
  ##' @param file a string containing location the likelihood output
  ##' @return vector of likelihood values
  ##' @author Bjarki Þór Elvarsson
  read.gadget.SS <- function(file='lik.out'){
    lik.out <- readLines(file)
    SS <- as.numeric(clear.spaces(strsplit(lik.out[length(lik.out)],
                                           '\t\t')[[1]][2]))
    return(SS)
  }

  ## read model
  main <- read.gadget.main(main.file)
  if(!is.null(main$printfile)){
    printfile <- read.gadget.printfile(main$printfile)
  } else {
    printfile <- NULL
  }
  likelihood <- read.gadget.likelihood(main$likelihoodfiles)
#  params.in <-read.gadget.parameters(params.file)
  
  ## initial run (to determine the initial run)
  main.init <- main
  main.init$printfile <- NULL
  main.init$likelihoodfiles <- paste(wgts,'likelihood.init',sep='/')
  write.gadget.likelihood(likelihood,file=paste(wgts,'likelihood.init',sep='/'))
  write.gadget.main(main.init,file=paste(wgts,'main.init',sep='/'))
  callGadget(s=1,main=paste(wgts,'main.init',sep='/'),
             o=paste(wgts,'lik.init',sep='/'),
             i=params.file,gadget.exe=gadget.exe)
  SS <- read.gadget.SS(paste(wgts,'lik.init',sep='/'))
  
  ## degrees of freedom approximated by the number of datapoints
  lik.dat <- read.gadget.data(likelihood)
  restr <- !(likelihood$weights$type %in%
             c('penalty','understocking','migrationpenalty'))
  
  ##' Survey indices get special treatment
  ##' @title survey index weight 
  ##' @param lik.dat Likelihood dataset
  ##' @return internal weights for the survey index components
  ##' @author Bjarki Thor Elvarsson
  sI.weights <- function(lik.dat){
    dat <- NULL
    for(comp in lik.dat$dat$surveyindices){
      dat <- rbind(dat,comp)
    }
    dat$comp <- rep(names(lik.dat$dat$surveyindices),lik.dat$df$surveyindices)
    dat$y <- log(dat$number)
    dat$year <- as.factor(dat$year)
    fit <- lm(y~year+length+step,dat)
    weights <- (lik.dat$df$surveyindices -
                tapply(dat$length,dat$comp,function(x) length(unique(x))))/
                  tapply(resid(fit),dat$comp,function(x) sum(x^2))
    return(weights)
  }
  
  restr.SI <- (likelihood$weights$type == 'surveyindices')  
  if(!rew.sI){
    run.string <- c(likelihood$weights$name[restr&(!restr.SI)&
                                            !(likelihood$weights$name %in%
                                              unlist(grouping))])
    run.string <- as.list(run.string)
    restr <- restr&(!restr.SI)
    sIw <- sI.weights(lik.dat)
    run.string$SI <- likelihood$weights$name[restr.SI]
  } else {
    run.string <- c(likelihood$weights$name[restr&
                                            !(likelihood$weights$name %in%
                                              unlist(grouping))])
    
    run.string <- as.list(run.string)
  }
  
  if(!is.null(grouping)){
    
    i <- 1
    run.string <-
      within(run.string,
             for(group in grouping){
               assign(sprintf('g%s',i),group)
               i <- i+1
             }
             )
    run.string$group <- NULL
    run.string$i <- NULL
  }
  
  ## Base run (with the inverse SS as weights)
  main.base <- main.init
  main.base$likelihoodfiles <- paste(wgts,'likelihood.base',sep='/')
  write.gadget.main(main.base,file=paste(wgts,'main.base',sep='/'))
  likelihood.base <- likelihood
  likelihood.base$weights$weight[restr] <- 1/SS[restr]
  if(!rew.sI)
    likelihood.base$weights$weight[restr.SI] <- sIw/sum(SS[restr.SI]*sIw)

  
  ##' Gadget set up stuff, needed for each component
  ##' @title run iterative
  ##' @param comp likelihood component
  ##' @return Sums of squares
  ##' @author Bjarki Thor Elvarsson
  run.iterative <- function(comp){
    likelihood <- likelihood.base
    which.comp <- likelihood$weights$name %in% comp
    likelihood$weights$weight[which.comp] <-
      10000*likelihood$weights$weight[which.comp]
    comp <- paste(comp,collapse='.')
    write.gadget.likelihood(likelihood,
                            file=paste(wgts,
                              paste('likelihood',comp,sep='.'),sep='/'))
    main <- main.base
    main$likelihoodfiles <- paste(wgts,paste('likelihood',comp,sep='.'),sep='/')
    write.gadget.main(main,file=paste(wgts,paste('main',comp,sep='.'),sep='/'))
    callGadget(l=1,
               main=paste(paste(wgts,'main',sep='/'),comp,sep='.'),
               i=params.file,
               p=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               opt=optinfofile,
               gadget.exe=gadget.exe,
               PBS=PBS,
               qsub.script=qsub.script,
               PBS.name=paste(wgts,comp,sep='/'))
    callGadget(s=1,
               main=paste(wgts,paste('main',comp,sep='.'),sep='/'),
               i=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               o=paste(wgts,paste('lik',comp,sep='.'),sep='/'),
               gadget.exe=gadget.exe,
               PBS=PBS,
               PBS.name=paste(wgts,comp,sep='/'))
#    SS.comp <- read.gadget.SS(paste(wgts,paste('lik',comp,sep='.'),sep='/'))
#    return(SS.comp)
  }
  ## 
  if(!resume.final){
    ## run the bloody thing
    if(run.serial)
      res <- lapply(run.string,run.iterative)
    else
      res <- mclapply(run.string,run.iterative)
  }

  ## Do we want to run the final optimisation (only used for debug purposes,
  ## and the check should be removed in later revisions)
  
  if(run.final){
    res <- lapply(run.string,
                  function(x)
                  read.gadget.SS(paste(wgts,
                                       paste('lik',
                                             paste(x,collapse='.'),
                                             sep='.'),sep='/')))                
    names(res) <- sapply(run.string,function(x) paste(x,collapse='.'))
    SS.table <- as.data.frame(t(sapply(res,function(x) x)))
    names(SS.table) <- likelihood.base$weights$name
    
    
    run.final <- function(comp){
      callGadget(l=1,
                 main=sprintf('%s/main.%s',wgts,comp),
                 i=params.file,
                 p=sprintf('%s/params.%s',wgts,comp),
                 opt=optinfofile,
                 gadget.exe=gadget.exe,
                 PBS=PBS,
                 PBS.name=paste(wgts,comp,sep='/'),
                 qsub.script=qsub.script)
      callGadget(s=1,
                 main=sprintf('%s/main.%s',wgts,comp),
                 i=sprintf('%s/params.%s',wgts,comp),
                 o=sprintf('%s/lik.%s',wgts,comp),
                 gadget.exe=gadget.exe)
    }

    ## read in the results from previous runs
    num.comp <- sum(restr)
    tmpSS <- NULL
    tmp.restr <- restr
    if(!is.null(grouping)){
      tmp.restr <- restr&(!(likelihood$weights$name %in% unlist(grouping)))
      for(group in grouping){
        tmpSS <- c(tmpSS,SS.table[paste(group,collapse='.'),
                                  likelihood$weights$name %in% group])
      }      
    }
    final.SS <- c(diag(as.matrix(SS.table[likelihood$weights$name[tmp.restr],
                                          tmp.restr])),tmpSS)
    final.SS <- final.SS[likelihood$weights$name[restr]]
    df <- rep(0,num.comp)
    lik.tmp <- likelihood$weights[restr,]
    for(i in 1:num.comp){
      df[i] <- lik.dat$df[[lik.tmp$type[i]]][[lik.tmp$name[i]]]
    }
    final.weights <- df/unlist(final.SS)
    if(!rew.sI){
      ind <- run.string$SI
      final.SI <- sIw/sum(sIw*SS.table[paste(ind,collapse='.'),ind])
      final.sIw <- unlist(c(final.weights,sIw))
      final.sIgroup <- unlist(c(final.weights,
                                lik.dat$df$surveyindices[ind]/
                                SS.table[paste(ind,collapse='.'),ind]))
      final.weights <- unlist(c(final.weights,final.SI))
    }

    ## final run
    write.files <- function(comp,weights){
      main <- main.base
      if(!is.null(printfile)){
        write.gadget.printfile(printfile,
                               sprintf('%s/%s.%s',wgts,'printfile',comp),
                               sprintf('%s/out.%s',wgts,comp))
        main$printfile <- sprintf('%s/%s.%s',wgts,'printfile',comp)
      }
      main$likelihoodfiles <- sprintf('%s/likelihood.%s',wgts,comp)
      write.gadget.main(main,sprintf('%s/main.%s',wgts,comp))
      
      likelihood <- likelihood.base
      likelihood$weights[names(weights),'weight'] <- weights
      write.gadget.likelihood(likelihood,
                              file=sprintf('%s/likelihood.%s',wgts,comp))
      dir.create(sprintf('%s/out.%s',wgts,comp),showWarnings=FALSE)
    }

      comp <- 'final'
      write.files(comp,final.weights)
    if(!rew.sI){
      write.files('sIw',final.sIw)
      write.files('sIgroup',final.sIgroup)
      comp <- as.list(c('final','sIw','sIgroup'))
    }
    

    lapply(comp,run.final)

  } else {
    comp <- NULL
  }
  return(list(comp=run.string,final=comp,wgts=wgts))
#  return(list(res=res,SS=SS.table,lik.dat=lik.dat))
}


##' Read the results from the gadget run, in particular the sums of squares
##' table which is useful for further analysis. TODO: add optional read
##' printfiles
##' @title read gadget results
##' @param comp list of likelihood components (with groupings if necessary)
##' @param final final postfix
##' @param wgts wgts folder (defaults to WGTS)
##' @param likelihood.file likelihood file for the model
##' @return a list containing the sums of squares table for the various likelihood components while heavily weighted and the likelihood data. 
##' @author Bjarki Þór Elvarsson
read.gadget.results <- function(comp,
                                final,
                                wgts='WGTS',
                                likelihood.file='likelihood'
                                ){

  read.gadget.SS <- function(file='lik.out'){
    lik.out <- readLines(file)
    SS <- as.numeric(clear.spaces(strsplit(lik.out[length(lik.out)],
                                           '\t\t')[[1]][2]))
    return(SS)
  }
  likelihood <- read.gadget.likelihood(likelihood.file)
  res <- lapply(comp,
                function(x)
                read.gadget.SS(paste(wgts,
                                     paste('lik',
                                           paste(x,collapse='.'),
                                           sep='.'),sep='/')))                
  names(res) <- sapply(comp,function(x) paste(x,collapse='.'))
  SS.table <- as.data.frame(t(sapply(res,function(x) x)))
  names(SS.table) <- likelihood$weights$name
  
  res <- lapply(final,
                function(x)
                read.gadget.SS(paste(wgts,
                                     paste('lik',
                                           paste(x,collapse='.'),
                                           sep='.'),sep='/')))                
  names(res) <- sapply(final,function(x) paste(x,collapse='.'))
  res <- as.data.frame(t(sapply(res,function(x) x)))
  names(res) <- names(SS.table)
  SS.table <- rbind(SS.table,res)
  lik.dat <- read.gadget.data(likelihood)
  return(list(SS=SS.table,lik.dat=lik.dat))
}


##' This function implements a crude sensitivity analysis of a gadget simulation
##' The test is run by changing each of the variables in your model
##' by up to +/- some percentage of the initial value. Often 
##' a higher resolution near the optimum is desired than is requried elsewhere.
##' @title Gadget sensitivity
##' @param file name of the input file with the initial point
##' @param outer.range The outer ranges of the parameter value considered, defined in terms of percentages.
##' @param outer.stepsize The increments/stepsize within in the outer range. 
##' @param inner.range Inner range where the finer mesh should be used
##' @param inner.stepsize Inner stepsize.
##' @param opt Will we be looking at only the optimized variables, or
##' all of them?
##' @param vars.all (logical) Will we be looking at all variables or
##' just some? 
##' @param var.names If only a few, which ones will they be? Can be
##' blank if we are using all variables
##' @param gadget.exe name of the gadget executable
##' @param sens.in name of the resulting gadget input file
##' @param lik.out a string containing the name of the likelihood
##' output file
##' @param within.bounds should gadget be restricted to 
##' @param main.file string naming the gadget main file used
##' @param sens.dir a string naming the folder where the result and
##' temporary files are stored. The funciton will create the folder if
##' it has not already been created.
##' @param calc.full (USE WITH CARE) should the the full hypercube of function values be calculated. Using this switch will increase the computation time required exponentially.
##' @param range The range of the sensitivity check
##' @param stepsize The stepsize used
##' @return results from lik.out
##' @author Bjarki Thor Elvarsson
sensitivity.gadget <- function(file='params.out',
                               outer.range=0.5,
                               outer.stepsize=0.05,
                               inner.range=0.05,
                               inner.stepsize=0.01,
                               opt=TRUE,
                               vars.all=TRUE,
                               var.names='',
                               gadget.exe='gadget',
                               sens.in='sens.in',
                               lik.out='lik.sens',
                               within.bounds=TRUE,
                               main.file='main',
                               sens.dir = 'SENS',
                               calc.full = FALSE
                               ){
  dir.create(sens.dir,showWarnings=FALSE)
  lik.out <- paste(sens.dir,lik.out,sep='/')
  params <- read.gadget.parameters(file=file)
  p.range <- 1 + sort(unique(c(seq(-outer.range,outer.range,by=outer.stepsize),
                               seq(-inner.range,inner.range,
                                   by=inner.stepsize))))
  restr <- TRUE
  if(opt){
    restr <- restr&(params$optimise==1)
  }
  if(!vars.all){
    restr <- restr&(params$switch %in% var.names)
  }
  num.points <- sum(restr)*length(p.range)
  param.table <- rep(params$value,each=num.points)
  dim(param.table) <- c(num.points,length(params$value))
  param.table <- as.data.frame(param.table)
  names(param.table) <- params$switch
  row.names(param.table) <- paste(rep(params$switch[restr],
                                      each=length(p.range)),
                                  1:length(p.range),sep='.')
  seat <- 0
  if(!calc.full){
    for(name in params$switch[restr]){
      param.res <- params$switch==name
      if(within.bounds){
        param.table[[name]][seat+1:length(p.range)] <-
          pmax(pmin(params$upper[param.res],
                    p.range*params$value[param.res]),
               params$lower[param.res])
      } else { 
        param.table[[name]][seat+1:length(p.range)] <-
          p.range*params$value[param.res]
      }
      seat <- seat+length(p.range)
    }
  } else {
    param.table <- 
      within(ls(),
             for(name in params$switch[restr]){
               param.res <- params$switch==name
               if(within.bounds){
                 assign(name,
                        pmax(pmin(params$upper[param.res],
                                  p.range*params$value[param.res]),
                             params$lower[param.res]))
               } else { 
                 assign(name,p.range*params$value[param.res])
               }
             })
    param.table$name <- NULL
    param.table <- expand.grid(param.table)
  }
  param.table <- unique(param.table)
  header <- paste('switches',paste(names(param.table),collapse='\t'),sep='\t')
  write(header,file=sens.in)
  write.table(param.table,file=sens.in,col.names=FALSE,append=TRUE,
              quote=FALSE,sep='\t',row.names=FALSE)
  main <- read.gadget.main(main.file)
  main$printfiles <- NULL
  write.gadget.main(main,file=sprintf('%s/%s.sens',sens.dir,main.file))
  callGadget(s=TRUE,i=sens.in,o=lik.out,
             p=paste(sens.dir,'sens.out',sep='/'),
             main=sprintf('%s/%s.sens',sens.dir,main.file),
             gadget.exe=gadget.exe)
  lik.sens <- read.gadget.lik.out(lik.out)
  sens.data <- lik.sens$data
  sens.data$parameter <- row.names(param.table)
#  attr(sens.data,'params') <- params
#  attr(sens.data,'comps') <- 
  class(sens.data) <- c('gadget.sens',class(sens.data))
  return(sens.data)
}

plot.gadget.sens <- function(sens,comp='score',ylimit=NULL,ncol=10,nrow=4){
  
  sens$parameter <- sapply(strsplit(sens$parameter,'.',fixed=TRUE),
                           function(x) paste(x[-length(x)],collapse='.'))
  lik.comps <- attr(sens,'Likelihood components')
  if(!(comp %in% c(lik.comps,'score')))
    stop(sprintf('Component %s not found in lik.comps',comp))
  params <- attr(sens,'Parameters')
  tmp <- ddply(sens,'parameter',
               function(x){
                 tmp <- cbind(x[x$parameter[1]],x[comp])
                 names(tmp) <- c('Value','score')
                 tmp
               })
  plo <- ggplot(tmp, aes(Value,score)) +
    geom_line() +
      facet_wrap(~parameter,scale='free') +
        xlab('') + ylab('') +
          opts(axis.text.x=theme_text(angle=-90,hjust=0))
                   
  
  return(plo)
  
}

##' Gadget phasing 
##' @title Gadget Phasing 
##' @param phase a dataframe where the columns indicate the parameters
##' that are to be optimised in that particular phase
##' @param params.in either a filename or gadget.parameters objecet
##' containing the initial value for the optimisation. 
##' @param main name of the main file used in the optimisation.
##' @param phase.dir output directory
##' @return final optimised parameter values
##' @author Bjarki Thor Elvarsson
gadget.phasing <- function(phase,params.in='params.in',main='main',phase.dir='PHASING'){
  dir.create(phase.dir, showWarnings = FALSE)
  if(class(params.in)=='character'){
    params.in <- read.gadget.parameters(params.in)
  } else if(!('gadget.parameters' %in% class(params.in))) {
    stop('params.in is not a valid gadget.parameters object')
  }
  tmp <- params.in$optimise
  for(p in names(phase)){
    params.in$optimise <- phase[[p]]
    write.gadget.parameters(params.in,sprintf('%s/params.%s',phase.dir,p))
    callGadget(l=1,main=main,i=sprintf('%s/params.%s',phase.dir,p),
               p=sprintf('%s/params.out.%s',phase.dir,p))
    params.in <- read.gadget.parameters(sprintf('%s/params.out.%s',phase.dir,p))
  }
  return(params.in)
}



##' <description>
##'
##' <details>
##' @title Bootstrap control 
##' @param bs.likfile Likelihood file from the DW
##' @param bs.samples number (or vector of numbers) indicating what bootstrap
##' samples should be used
##' @param main Main file for the gagdet model
##' @param optinfofile optinfofile used in the optimization.
##' @param bs.wgts folder containing the resulting reweights
##' @param bs.data folder containing the bootstrap dataset obtain from the DW
##' @param params.file 
##' @param rew.sI should the survey indices be reweighted seperately
##' @param grouping list of grouped likelihood components
##' @param qsub.script name of the qsub script if the calculations is meant to be run on a cluser
##' @param run.final logical, is this the final run or weighting run. 
##' @param PBS logical, is this a cluster run? 
##' @return NULL
##' @author Bjarki Thor Elvarsson
gadget.bootstrap <- function(bs.likfile = 'likelihood.bs',
                             bs.samples = 1,
                             main='main',
                             optinfofile='optinfo',
                             bs.wgts='BS.WGTS',
                             bs.data=NULL,
                             params.file = 'params.in',
                             rew.sI = FALSE,
                             grouping = NULL,
                             qsub.script = 'bootstrap.sh',
                             run.final = FALSE,
                             PBS=TRUE
                             ){
  
  dir.create(bs.wgts,showWarnings=FALSE)
  main <- read.gadget.main(main)
  bs.lik <- read.gadget.likelihood(bs.likfile)
    
  foreach(i=bs.samples) %dopar% {
    dir.create(sprintf('%s/BS.%s',bs.wgts,i),showWarnings=FALSE)
    bs.lik.file <- sprintf('%s/BS.%s/likelihood',bs.wgts,i)
    bs.main.file <- sprintf('%s/BS.%s/main',bs.wgts,i)
    if(!run.final) {
      bs.main <- main
      bs.main$likelihoodfiles <- bs.lik.file
      write.gadget.main(bs.main,bs.main.file)
      write.gadget.likelihood(bs.lik,bs.lik.file,bs.data,sprintf('.%s',i))
      tmp <- gadget.iterative(main.file=bs.main.file,
                              params.file = params.file,
                              grouping = grouping,
                              optinfofile = optinfofile,
                              rew.sI = rew.sI,
                              PBS = PBS,
                              qsub.script = qsub.script,
                              wgts = sprintf('%s/BS.%s',bs.wgts,i),
                              run.final = FALSE,
                              run.serial = TRUE)
      if(PBS)
        write(sprintf('# bootstrap sample %s',i),file=qsub.script,append=TRUE)
      if(i > 100 & PBS)
        write('sleep 6m',file=qsub.script,append=TRUE)
      else
        print(sprintf('# bootstrap sample %s',i))
      
    } else {
      tmp <- gadget.iterative(main.file = bs.main.file,
                              params.file = params.file,
                              grouping = grouping,
                              PBS = PBS,
                              optinfofile = optinfofile,
                              rew.sI = rew.sI,
                              qsub.script = qsub.script,
                              wgts = sprintf('%s/BS.%s',bs.wgts,i),
                              resume.final = TRUE,
                              run.final = TRUE)
      if(PBS)
        write(sprintf('# bootstrap sample %s',i),file=qsub.script,append=TRUE)
      if(i > 100 & PBS)
        write('sleep 6m',file=qsub.script,append=TRUE)
      else
        print(sprintf('# bootstrap (final) sample %s',i))

    }
  }
  return(NULL)
}
