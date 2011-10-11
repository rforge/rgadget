##' This function attempts to read in the gadget output files defined in
##' printfiles. This is a quick and dirty implementation that has been 
##' designed to read a few nice examples, so it may work for some instances
##' and it may not. It assumes that the last line containing a comment ';'
##' is the line describing the column names and is the last comment line.
##' @title Read gadget printfiles
##' @param path a character string with the name of the folder containing the printfiles
##' @return a list containing the data that has been read in named after the files found in path.
read.printfiles <- function(path='.'){
  read.printfile <- function(file){
     tmp <- readLines(file)
     skip <- max(grep(tmp,';'))
     header <- unlist(sapply(strsplit(tmp[skip],' '),
                             function(x) strsplit(x[2],'-')))
     data <- read.table(file,comment.char=';',header=FALSE)
     if(length(names(data)) != length(header)){
       warning(sprintf('Error in read.printfile -- Header could no be read from file %s',file))
     } else {
       names(data) <- header
     }
     return(data)
  }
  out.files <- list.files(path=path)
  printfiles <- within(list(),
                       for(printfile in out.files){
                         assign(printfile,
                                read.printfile(paste(path,
                                                     printfile,
                                                     sep='/')))
                       })
  printfiles$printfile <- NULL
  return(printfiles)
}
##' This functions reads the likelihood (input) file for gadget. The format of
##' the likelihood file is described in gadget's user manual. 
##' @title Read likelihood
##' @param files a vector of character strings containing the names of the likelihood files 
##' @return object of class gadget.likelihood, i.e. a list containing the various likelihood components
##' @author Bjarki Þór Elvarsson
read.gadget.likelihood <- function(files='likelihood'){
  lik <- NULL
  for(file in files)
    lik <- c(lik,sub(' +$','',gsub('\t',' ',readLines(file))))
  lik <- lik[lik!='']
  lik <- lik[!grepl(';',substring(lik,1,1))]
  lik <- sapply(strsplit(lik,';'),function(x) sub(' +$','',x[1]))
  comp.loc <- grep('component',lik)
  name.loc <- comp.loc+3
  weights <- NULL
  common <- c('name','weight','type','datafile','areaaggfile','lenaggfile',
              'ageaggfile')
  tmp.func <- function(comp){
    loc <- grep(tolower(comp),tolower(lik[name.loc]))  
    if(sum(loc)==0){
      return(NULL)
    }else {
      dat <- list()
      for(dd in loc){
        if(dd < length(comp.loc)) {
          restr <- (comp.loc[dd] + 1):(comp.loc[dd+1]-1)
        } else {
          restr <- 1:length(lik) > comp.loc[dd]
        }
        tmp <- sapply(strsplit(sapply(strsplit(lik[restr],'[ \t]'),
                                      function(x) {
                                        paste(x[!(x==''|x=='\t')],
                                              collapse=' ')
                                      }),' '),
                      function(x) as.character(x))
        if(class(tmp)!='list'){
          names.tmp <- head(tmp,1)
          tmp <- as.data.frame(tmp,stringsAsFactors=FALSE)[2,]
          names(tmp) <- names.tmp
          row.names(tmp) <- tmp$name
          tmp$type <- tolower(tmp$type)
        } else {
          names.tmp <- sapply(tmp,function(x) x[1])
          tmp <- sapply(tmp,function(x) paste(x[-1], collapse='\t'))
          names(tmp) <- names.tmp
          tmp <- as.data.frame(t(tmp),stringsAsFactors=FALSE)
          tmp$type <- tolower(tmp$type)
        }
        
        if(tolower(comp)=='understocking'|tolower(comp)=='migrationpenalty'){
          tmp$datafile <- ''
        }
        if(is.null(tmp$areaaggfile))
          tmp$areaaggfile <- ''
        if(is.null(tmp$lenaggfile))
          tmp$lenaggfile <- ''
        if(is.null(tmp$ageaggfile))
          tmp$ageaggfile <- ''

        weights <<-  rbind(weights,
                           tmp[common])
#                           as.data.frame(t(unlist(tmp[common])),
#                                         stringsAsFactors=FALSE))
        if(tolower(comp)=='understocking'|tolower(comp)=='migrationpenalty'){
          tmp$datafile <- NULL
        }

        if(tmp$areaaggfile=='')
          tmp$areaaggfile <- NULL
        if(tmp$lenaggfile=='')
          tmp$lenaggfile <- NULL
        if(tmp$ageaggfile=='')
          tmp$ageaggfile <- NULL
        
        tmp$weight <- NULL
        dat <- within(dat,assign(tmp$name,tmp))
        
        comp.loc <- c(comp.loc,length(lik) + 1)
      }
      
      if(length(unique(comp.loc[loc+1]-comp.loc[loc]))<2 )
        dat <-
          as.data.frame(t(sapply(dat,function(x) unlist(x))),stringsAsFactors=FALSE)
      return(dat)
    }
  }
  ## understocking

  likelihood <- list(penalty = tmp.func('penalty'),
                     understocking = tmp.func('understocking'),
                     surveyindices = tmp.func('surveyindices'),
                     catchdistribution = tmp.func('catchdistribution'),
                     catchstatistics = tmp.func('catchstatistics'),
                     surveydistribution = tmp.func('surveydistribution'),
                     stomachcontent = tmp.func('stomachcontent'),
                     recaptures = tmp.func('recaptures'),
                     recstatistics = tmp.func('recstatistics'),
                     migrationpenalty = tmp.func('migrationpenalty'),
                     catchinkilos = tmp.func('catchinkilos'))
  likelihood$weights <- weights
  row.names(likelihood$weights) <- weights$name
  likelihood$weights$weight <- as.numeric(weights$weight)
  likelihood <- likelihood[c('weights',unique(likelihood$weights$type))]
  class(likelihood) <- c('gadget.likelihood',class(likelihood))
  return(likelihood)
}
##' Write a likelihood object to file
##' @title Write likelihood
##' @param lik object of class gadget.likelihood
##' @param file name of the likelihood file
##' @param data.folder 
##' @param bs.sample 
##' @return character string corresponding to the likelihood file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.likelihood <- function(lik,file='likelihood',
                                    data.folder=NULL, bs.sample=NULL){
  lik.text <- sprintf('; Likelihood file - created in Rgadget\n; %s',file)
  weights <- lik$weights
  lik$weights <- NULL
  weights$type <- NULL
  weights$datafile <- NULL
  weights$lenaggfile <- NULL
  weights$areaaggfile <- NULL
  weights$ageaggfile <- NULL
  for(comp in lik){
    if(class(comp) == 'data.frame'){
      if(!is.null(data.folder)){
        comp$datafile <- paste(data.folder,comp$datafile,sep='/')
      }
      comp <- merge(weights,comp,by='name',sort=FALSE)
      comp.text <- paste(names(comp),t(comp))
      dim(comp.text) <- dim(t(comp))
      comp.text <- rbind('[component]',comp.text,';')
      lik.text <- paste(lik.text,
                        paste(comp.text,
                            collapse='\n'),
                        sep='\n')
    } else {
      for(sub.comp in comp){
        if(!is.null(data.folder)){
          sub.comp$datafile <- paste(data.folder,sub.comp$datafile,sep='/')
        }
        sub.comp <- merge(weights,sub.comp,by='name',sort=FALSE)
        comp.text <- paste(names(sub.comp),t(sub.comp))
        dim(comp.text) <- dim(t(sub.comp))
        comp.text <- rbind('[component]',comp.text,';')
        lik.text <- paste(lik.text,
                          paste(comp.text,
                                collapse='\n'),
                          sep='\n')
      }
    }
  }
  if(!is.null(bs.sample))
    write(sprintf(lik.text,bs.sample),file=file)
  else
    write(lik.text,file=file)
  invisible(lik.text)
}

##' <description>
##'
##' <details>
##' @title 
##' @param lik1 
##' @param lik2 
##' @return 
##' @author Bjarki Thor Elvarsson
merge.gadget.likelihood <- function(lik1,lik2){
  tmp <- within(list(),
                for(comp in unique(c(names(lik1),names(lik2)))){
                  assign(comp,
                         unique(rbind(lik1[[comp]],lik2[[comp]])))
                })
  class(tmp) <- c('gadget.likelihood',class(tmp))
  return(tmp)
}
##' <description>
##'
##' <details>
##' @title 
##' @param likelihood 
##' @param comp 
##' @return 
##' @author Bjarki Thor Elvarsson
get.gadget.likelihood <- function(likelihood,comp){
  weights <- likelihood$weights[likelihood$weights$name %in% comp,]
  tmp <-
    within(list(),
           for(type in weights$type){
             assign(type,
                    likelihood[[type]][likelihood[[type]][['name']] %in% comp,])
           }
           )
  tmp$type <- NULL
  tmp$weigths <- weights
  class(tmp) <- c('gadget.likelihood',class(tmp))
  return(tmp)
}


new.gadget.main <- function(){
  main <-
    list(timefile = '',
         areafile = '',
         printfiles = '',
         stockfiles = '',
         tagfiles = '',
         otherfoodfiles = '',
         fleetfiles = '',
         likelihoodfiles = '')
  class(main) <- c('gadget.main',class(main))
  return(main)
}

##' Read gadget's main file
##' @title Read main
##' @param file main file location
##' @return object of class gadget.main
##' @author Bjarki Þór Elvarsson
read.gadget.main <- function(file='main'){
  main <- sub(' +$','',readLines(file))
  main <- main[main!='']
  main <- main[!grepl(';',substring(main,1,1))]
  main <- sapply(strsplit(main,';'),function(x) x[1])
  main <- clear.spaces(main)
  tmp <- sapply(main[sapply(main,length)!=1],function(x) x[2:length(x)])
  names(tmp) <-  sapply(main[sapply(main,length)!=1],function(x) x[1])
  main <- as.list(tmp)
  class(main) <- c('gadget.main',class(main))
  return(main)
}
##' Write gadget.main object to file
##' @title Write main
##' @param main gadget.main object
##' @param file name of main file 
##' @return text of the main file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.main <- function(main,file='main'){
  main.text <- sprintf('; main file for gadget - created in Rgadget\n; %s - %s',
                       file,date())
  if(is.null(main$printfiles)){
    main$printfiles <- '; no printfile supplied'  
  }
  main.text <-
    paste(main.text,
          paste('timefile',main$timefile),
          paste('areafile',main$areafile),
          paste('printfiles',paste(main$printfiles,collapse='\t')),
          '[stock]',
          paste('stockfiles',paste(main$stockfiles,collapse='\t')),
          ifelse(is.null(main$tagfiles), #| main$tagfiles == '',
                 '[tagging]',
                 paste('[tagging]\ntagfiles',paste(main$tagfiles,
                                                   collapse='\t'))),
          ifelse(is.null(main$otherfoodfiles), #| main$otherfoodfiles == '',
                 '[otherfood]',
                 paste('[otherfood]\notherfoodfiles',
                       paste(main$otherfoodfiles,collapse='\t'))),
          ifelse(is.null(main$likelihoodfiles), # | main$likelihoodfiles == '',
                 '[fleet]',
                 paste('[fleet]\nfleetfiles',
                       paste(main$fleetfiles,collapse='\t'))),
          '[likelihood]',
          paste('likelihoodfiles',
                paste(main$likelihoodfiles,collapse='\t')),
          sep='\n')
  write(main.text,file=file)
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
##' @return dataframe
##' @author Bjarki Þór Elvarsson
read.gadget.parameters <- function(file='params.in'){
  params <- read.table(file,header=TRUE,
                       comment.char=';',
                       stringsAsFactors=FALSE)
  row.names(params) <- params$switch
  ## digg through the data written in the header
  header <- readLines(file)
  header <- header[grepl(';',substring(header,1,1))]

  num.func <- function(pre){
    post <- ' function evaluations'
    num <- as.numeric(gsub(post,'',gsub(pre,'',header[grepl(pre,header)])))
    return(num)
  }

  ## Number of function evaluations
  sim.func.str <- '; Simulated Annealing algorithm ran for '
  sim.pos <- grep(sim.func.str,header)
  
  hook.func.str <- '; Hooke & Jeeves algorithm ran for '
  hook.pos <- grep(hook.func.str,header)
  
  bfgs.func.str <- '; BFGS algorithm ran for '
  bfgs.pos <- grep(bfgs.func.str,header)
  

  
  ## final likelihood values from each component
  lik.func <- function(i){
    as.numeric(gsub('; and stopped when the likelihood value was ','',
                    header[i]))
  }
  
  ## convergence
  conv.func <- function(i){
    error <- '; because an error occured during the optimisation'
    converged <- '; because the convergence criteria were met'
    maxiter <-
      '; because the maximum number of function evaluations was reached'
    ifelse(header[i]==error,'Error in optimisation',
           ifelse(header[i]==converged,'Convergence criteria were met',
                  ifelse(header[i]==maxiter,'Maximum number of iterations',
                         'No information')))
  }
  
  
  tmp <- list(simann=data.frame(numFunc=num.func(sim.func.str),
                lik.val=lik.func(sim.pos+1),
                convergence=conv.func(sim.pos+2),
                stringsAsFactors=FALSE),
              hooke=data.frame(numFunc=num.func(hook.func.str),
                lik.val=lik.func(hook.pos+1),
                convergence=conv.func(hook.pos+2),
                stringsAsFactors=FALSE),
              bfgs=data.frame(numFunc=num.func(bfgs.func.str),
                lik.val=lik.func(bfgs.pos+1),
                convergence=conv.func(bfgs.pos+2),
                stringsAsFactors=FALSE))
  class(params) <- c('gadget.parameters',class(params))
  attr(params,'optim.info') <- tmp
  return(params)
}
##' Write gadget input parameters
##' @title Write params
##' @param params params dataframe
##' @param file a string naming the file to write to
##' @return a string containing the text of the params file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.parameters <- function(params,file='params.out'){
  input.text <-
    paste("; input file for the gadget model",
          "; created automatically from Rgadget",
          sprintf('; %s - %s',file,date()),
          paste(names(params),collapse='\t'),
          sep='\n')
  write(input.text,file)
  write.table(params,file=file,
              quote=FALSE, row.names=FALSE, col.names=FALSE,
              append=TRUE, sep="\t")
}
##' Read gadget printfile
##' @title Read gadget printfile
##' @param file string containing the path to the printfile
##' @return list of the prinfile components.
##' @author Bjarki Thor Elvarsson
read.gadget.printfile <- function(file='printfile'){
  printfile <- strip.comments(file)
  comp.loc <- grep('component',printfile)
  tmp.func <- function(restr){
    names.tmp <- sapply(printfile[restr],       
                        function(x) x[1])
    tmp <- lapply(sapply(printfile[restr],                                
                         function(x) x[-1]),unlist)
    names(tmp) <- names.tmp
    return(tmp)
  }
  print <- within(list(),
                  for(i in 1:length(comp.loc)){
                    if(i < length(comp.loc)){
                      restr <- (comp.loc[i]+1):(comp.loc[i+1]-1)
                    } else {
                      restr <- (comp.loc[i]+1):length(printfile)
                    }
                    tmp <- tmp.func(restr)                    
                    comp.name <- sapply(strsplit(tmp$printfile,'/'),
                                        function(x) x[length(x)])
                    
                    assign(comp.name,tmp)
                  }
                  )
  print$i <- NULL
  print$restr <- NULL
  print$tmp <- NULL
  print$comp.name <- NULL
  
  return(print)
}
##' Write the gadget prinfile to file, optionally changing the output directory
##' of the printfile components.
##' @title Write Gadget printfile
##' @param print printfile object
##' @param file string containing the desired location of the printfile
##' @param output.dir where should the output from the prinfile components be written, defaults to 'out'.
##' @return (invisible) text of the printfile if desired.
##' @author Bjarki Thor Elvarsson
write.gadget.printfile <- function(print,file='prinfile',output.dir='out'){
  print.text <- sprintf('; Printfile for gadget, created by Rgadget\n; %s',file)
  for(name in names(print)){
    tmp <- print[name][[name]]
    tmp[['printfile']] <- paste(output.dir,name,sep='/')
    print.text <- paste(print.text,
                        ';\n[component]',
                        paste(names(tmp),sapply(tmp,function(x) paste(x,collapse='\t')),
                              sep='\t',collapse='\n'),
                        sep='\n')
  }
  write(print.text,file)
  invisible(print.text)
}


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
  SS.table <- rbind(SS.table,
                    as.data.frame(t(sapply(res,function(x) x)))
                    )
  lik.dat <- read.gadget.data(likelihood)
  return(list(SS=SS.table,lik.out=lik.out))
}


##' Read data used by the various components
##' @title Read likelihood data
##' @param likelihood object of class gadget.likelihood
##' @return list of dataframes and degress of freedom
##' @author Bjarki Þór Elvarsson
read.gadget.data <- function(likelihood){
  read.agg <- function(x){
    if(!is.null(x))
      return(read.table(x,stringsAsFactors=FALSE,comment.char=';')[,1])
#      return(sapply(strsplit(readLines(x),'[\t ]'),function(x) x[1]))
    else
      return(NULL)
  }
  read.func <- function(x){
    x <- as.data.frame(t(x),stringsAsFactors=FALSE,comment.char=';')

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
                                           c('weights','penalty',
                                             'understocking',
                                             'migrationpenalty'))])) {

                      assign(comp.type,
                             apply(likelihood[[comp.type]],1,read.func))
                    }
                    
                    )
  lik.dat$comp.type <- NULL
  df <- lapply(lik.dat,function(x)
               sapply(x,function(x) dim(x[x[,dim(x)[2]]>0,])[1]))
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
write.gadget.optinfo<-function(optinfo,file='optinfofile'){
  opt.text <- 
    paste("; optimisation file for gadget",
          "; created in R-gadget",
          sprint('; %s - %s',file,date()),
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
  write(opt.text,file=file)
  invisible(opt.text)
}



##' Read in the gadget likelihood output.
##' @title Read gadget lik.out 
##' @param file string containing the name of the file
##' @return a list containing the swicthes (names of variable), weigths
##' (líkelihood components) and data (dataframe with the parameter values,
##' likelihood component values and the final score.
##' @author Bjarki Thor Elvarsson
read.gadget.lik.out <- function(file='lik.out'){
  lik <- readLines(file)
  i <- grep("Listing of the switches",lik)
  i1 <- grep("Listing of the likelihood components",lik)
  i2 <- grep("Listing of the output from the likelihood",lik)
  switches <- lapply(strsplit(lik[(i+1):(i1-2)],'\t'),unique)
  names(switches) <- sapply(switches,function(x) x[1])
  switches <- lapply(switches,function(x) x[-1])

  weights <- t(sapply(strsplit(lik[(i1+3):(i2-2)],'\t'),function(x) x))
  weights <- as.data.frame(weights,stringsAsFactors=FALSE)
  weights$V2 <- as.numeric(weights$V2)
  weights$V3 <- as.numeric(weights$V3)
  names(weights) <- c('Component','Type','Weight')
  
  data <- read.table(file,skip=(i2+1))
  names(data) <- c('iteration',names(switches),weights$Component,'score')
  lik.out <- list(switches=switches,weights=weights,data=data)
  class(lik.out) <- c('gadget.lik.out',class(lik.out))
  return(lik.out)
}



##' Helper function created to clear out all comments (indicated by ';') and 
##' unwanted spaces from gadget input and output files.
##' @title Strip comments
##' @param file location of the gadget input file
##' @return list containing the lines from the file stripped of unwanted text.
##' @author Bjarki Thor Elvarsson
strip.comments <- function(file='main'){
  main <- sub(' +$','',readLines(file))
  main <- main[main!='']
  main <- main[!grepl(';',substring(main,1,1))]
  main <- sapply(strsplit(main,';'),function(x) x[1])
  main <- clear.spaces(main)
  return(main)
}

##' <description>
##'
##' <details>
##' @title 
##' @param main.file 
##' @return 
##' @author Bjarki Thor Elvarsson
read.gadget.model <- function(main.file='main'){
  gadget.model <-
    within(list(),
           main <- read.gadget.main(main.file),
           time <- read.gadget.time(main$timefile),
           area <- read.gadget.area(main$areafile),
           print <- read.gadget.printfile(main$printfile),
           stocks <- read.gadget.stockfiles(main$stockfiles),
           tagging <- read.gadget.tagfiles(main$tagfiles),
           otherfood <- read.gadget.otherfood(main$otherfoodfiles),
           fleets <- read.gadget.fleet(main$fleetfiles),
           likelihood <- read.gadget.likelihood(main$likelihoodfiles)
           )
  class(gadget.model) <- c('gadget.model',class(gadget.model))
}
##' <description>
##'
##' <details>
##' @title 
##' @param stock.files 
##' @return 
##' @author Bjarki Thor Elvarsson
read.gadget.stockfiles <- function(stock.files){
  tmp.func <- function(sf){
    stock <- strip.comments(sf)
    st.names <- sapply(stock[1:9],function(x) x[1])
    st <- sapply(stock[1:9],function(x) x[2])
    names(st) <- st.names

    ## pop from list
    stock[1:9] <- NULL
    
    ## check 'doesgrow switch
    if(stock[[1]][2]==0){ 
      growthfunction <- NULL
      stock[1] <- NULL
    } else if(stock[[2]][2] == 'weightjones'){
      growthfunction <-
        list(wgrowthparameters = stock[[3]][-1],
             lgrowthparameters = stock[[4]][-1])
      stock[1:4] <- NULL
    } else if(stock[[2]][2] == 'weightvbexpanded'){
      growthfunction <-
        list(wgrowthparameters = stock[[3]][-1],
             lgrowthparameters = stock[[4]][-1],
             yeareffect = stock[[5]][-1],
             stepeffect = stock[[6]][-1],
             areaeffect = stock[[7]][-1])
      stock[1:7] <- NULL
    } else if(stock[[2]][2] %in% c('lengthvb','lengthpower')){
      growthfunction <-
        list(growthparameters = stock[[3]][-1],
             weightparameters = stock[[4]][-1])
      stock[1:4] <- NULL
    } else {
      growthfunction <-
        list(growthparameters = stock[[3]][-1])
      stock[1:3] <- NULL
    }
    implementation <- lapply(stock[1:2],function(x) x[-1])
    names(implementation) <- lapply(stock[1:2],function(x) x[1])
    stock[1:2] <- NULL
    growth <- list(growthfunction=growthfunction,
                   implementation=implementation)
    
    st$naturalmortality <- stock[[1]][-1]
    stock[1] <- NULL

    ## iseaten
    if(stock[[1]][2]==0){
      prey.info <- NULL
      stock[1] <- NULL
    } else {
      prey.info <- lapply(stock[2:3],function(x) x[-1])
      names(prey.info) <- lapply(stock[2:3],function(x) x[1])
      stock[1:2] <- NULL
    }

    ## doeseat
    if(stock[[1]][2]==0){
      pred.info <- NULL
      stock[1] <- NULL
    } else {
      stock[1] <- NULL
      pref <- grep('preference',stock)
      suit <- grep('suitability',stock)
      maxcon <- grep('maxconsumption',stock)
      
      suitability <- lapply(stock[2:(pref-1)],function(x) x[-1])
      names(suitability) <- sapply(stock[2:(pref-1)],function(x) x[1])

      preference <- t(sapply(stock[pref:(maxcon-1)],function(x) x))
      names(preference) <- c('preyname','preference')
      pred.info <-
        list(suitability=suitability,
             preference=preference,
             maxconsumption=stock[[maxcon]][-1])
      stock[1:maxcon] <- NULL
    }

    
    
    
  }
  stocks <- within(list(),
                   for(sf in stock.files){
                     tmp.func(sf)
                   })
  stocks$sf <- NULL
}

##' <description>
##'
##' <details>
##' @title 
##' @param area.file 
##' @return 
##' @author Bjarki Thor Elvarsson
read.gadget.area <- function(area.file='area'){
  area <- strip.comments(area.file)
  areas <- area[[1]][-1]
  size <- area[[2]][-1]
  temperature <-
    as.data.frame(t(sapply(area[-c(1:3)],function(x) as.numeric(x))))
  names(temperature) <- c('year','step','area','temperature')
  area <- list(areas=areas,size=size,temperature=temperature)
  class(area) <- c('gadget.area',class(area))
  return(area)
}
##' <description>
##'
##' <details>
##' @title 
##' @param area 
##' @param file 
##' @return 
##' @author Bjarki Thor Elvarsson
write.gadget.area <- function(area,file='area'){
  header <- sprintf('; time file created in Rgadget\n; %s - %s',file,date())
  area.file <-
    paste(header,
          paste('areas',paste(area$areas,collapse=' '),sep='\t'),
          paste('size',paste(area$size,collapse=' '),sep='\t'),
          'temperature',
          '; year - step - area - temperature',
          sep='\n')
  write(area.file,file=file)
  write.table(area$temperature,file=file,col.names=FALSE,append=TRUE,
              quote=FALSE,sep='\t',row.names=FALSE)
}
##' <description>
##'
##' <details>
##' @title 
##' @param time.file 
##' @return 
##' @author Bjarki Thor Elvarsson
read.gadget.time <- function(time.file='time'){
  time <- strip.comments(time.file)
  time.names <- sapply(time,function(x) x[1])
  time <- sapply(time,function(x) as.numeric(x[-1]))
  names(time) <- time.names
  if(sum(time$notimesteps[-1])!=12)
    warning('Error in timefile - notimesteps does not sum to 12')
  if(length(time$notimesteps[-1])!=time$notimesteps[1])
    warning('Error in timefile - notimesteps does not contain the right number of timesteps')
  time$notimesteps <- time$notimesteps[-1]
  class(time) <- c('gadget.time',class(time))
  return(time)
}

##' <description>
##'
##' <details>
##' @title 
##' @param time 
##' @param file 
##' @return 
##' @author Bjarki Thor Elvarsson
write.gadget.time <- function(time,file='time'){
  header <- sprintf('; time file created in Rgadget\n; %s - %s',file,date())
  time.file <-
    paste(header,
          paste('firstyear',time$firstyear,sep='\t'),
          paste('firststep',time$firststep,sep='\t'),
          paste('lastyear',time$lastyear,sep='\t'),
          paste('laststep',time$laststep,sep='\t'),
          paste('notimesteps',
                paste(length(time$notimesteps),
                      paste(time$notimesteps,collapse=' ')),
                sep='\t'),
          sep='\n')
  write(time.file,file=file)
}

##' Gadget Penalty file
##' @param file name of the file that is to contain the penalty
write.gadget.penalty <- function(file='penaltyfile'){
  penalty <- paste("; penalty file for the gadget example",
                   sprintf('; %s created at %s using Rgadget',file,date()),
                   "; switch - power - lower - upper",
                   "default\t2\t10000\t10000 ; defaults",
                   sep='\n')
  write(penalty,file=file)
}
